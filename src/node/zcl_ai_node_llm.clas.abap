CLASS zcl_ai_node_llm DEFINITION
  PUBLIC
  INHERITING FROM zcl_ai_node_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        name TYPE string.
    METHODS zif_ai_node~get_configuration REDEFINITION.
    METHODS zif_ai_node~set_configuration REDEFINITION.
  PROTECTED SECTION.
*    DATA name TYPE string VALUE 'LLM Node'.
    METHODS do_execute REDEFINITION.
  PRIVATE SECTION.
    DATA mo_llm_client TYPE REF TO zcl_ai_llm_client.
    " Parser structure for configuration
    TYPES: BEGIN OF ts_llm_node_config,
             name TYPE string,
           END OF ts_llm_node_config.

ENDCLASS.



CLASS zcl_ai_node_llm IMPLEMENTATION.
  METHOD constructor.
    super->constructor( node_name = name ).
    " Create real LLM client (SAP ISLM / AI Core)
    mo_llm_client = NEW zcl_ai_llm_client( ).
  ENDMETHOD.

  METHOD do_execute.
    " TASK US 1.2.1.C: LLM Node Error Handling & Resilience

    DATA(lv_system_prompt) = `You are an SAP ABAP AI agent. Help the user with SAP-related tasks.`.
    DATA lv_user_prompt TYPE string.
    lv_user_prompt = zcl_ai_utils=>messages_to_string( state-messages ).


    TRY.
        " 1. Attempt to call the LLM client stub
        "DATA(response) = zcl_ai_llm_client_stub=>generate_completion( prompt = state-messages ).

        " 2) Call LLM client (REAL SAP AI Core)
        DATA(lv_llm_output) = mo_llm_client->generate_completion(
          iv_system_prompt = lv_system_prompt
          iv_user_prompt   = lv_user_prompt ).
        " If successful, append the response to the state messages

        APPEND VALUE #( role = zif_ai_types=>gc_role_assistant content = lv_llm_output ) TO state-messages.

        " 2. Catch the custom AI Agent Errors (Timeout or API Failure)
      CATCH zcx_ai_agent_error INTO DATA(lx_error).

        DATA lv_error_message TYPE string.
        " Retrieve the custom detail message from the exception
        DATA(lv_detail_text) = lx_error->message.

        " 3. Logic to differentiate errors and set workflow status
        IF lx_error->error_id = zcx_ai_agent_error=>timeout.
          " If Timeout: Set status to RETRYING (requires orchestrator logic to handle retry)
          state-status = zif_ai_types=>gc_workflow_status_retrying.
          lv_error_message = |AI Service TIMEOUT. Retrying... Details: { lv_detail_text }|.

        ELSEIF lx_error->error_id = zcx_ai_agent_error=>api_error.
          " If API Error: Set status to ERROR (workflow terminates)
          state-status = zif_ai_types=>gc_workflow_status_error.
          lv_error_message = |AI Service API ERROR. Workflow terminated. Details: { lv_detail_text }|.

        ELSE.
          " Generic custom error fallback
          state-status = zif_ai_types=>gc_workflow_status_error.
          lv_error_message = |AI Agent Error (Generic). Details: { lv_detail_text }|.

          " since no error handling policy and error should not be sent to the llm so agent ends here
          state-branch_label = 'END'.
        ENDIF.

        " Append the human-readable error message to the state
        APPEND VALUE #( role = zif_ai_types=>gc_role_error content = lv_error_message ) TO state-messages.



        " 4. Catch any critical, unhandled ABAP runtime errors
      CATCH cx_root INTO DATA(lx_abap_error).
        state-status = zif_ai_types=>gc_workflow_status_error.
        DATA(lv_abap_error_message) = lx_abap_error->get_text( ).
        APPEND VALUE #( role = zif_ai_types=>gc_role_error content =  |CRITICAL UNHANDLED ERROR: { lv_abap_error_message }| ) TO state-messages.

        " since no error handling policy and error should not be sent to the llm so agent ends here
        state-branch_label = 'END'.


    ENDTRY.
  ENDMETHOD.

  METHOD zif_ai_node~get_configuration.
    DATA llm_node_config TYPE ts_llm_node_config.
    llm_node_config-name = node_name.
    configuration = xco_cp_json=>data->from_abap( llm_node_config )->to_string( ).
  ENDMETHOD.

  METHOD zif_ai_node~set_configuration.
    " Use a JSON parser to read configuration and access private and protected fields
    DATA llm_node_config TYPE ts_llm_node_config.
    xco_cp_json=>data->from_string( configuration )->write_to(
      REF #( llm_node_config )
    ).
    node_name = llm_node_config-name.
  ENDMETHOD.

ENDCLASS.
