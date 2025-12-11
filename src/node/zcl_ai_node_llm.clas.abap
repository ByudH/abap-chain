CLASS zcl_ai_node_llm DEFINITION
  PUBLIC
  INHERITING FROM zcl_ai_node_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        node_id  TYPE zif_ai_types=>ty_node_id
        agent_id TYPE zif_ai_types=>ty_agent_id.
  PROTECTED SECTION.
    METHODS do_execute REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ai_node_llm IMPLEMENTATION.
  METHOD constructor.
    super->constructor( node_id = node_id agent_id = agent_id ).
  ENDMETHOD.

  METHOD do_execute.
    " TASK US 1.2.1.C: LLM Node Error Handling & Resilience

    TRY.
        " 1. Attempt to call the LLM client stub
        DATA(response) = zcl_ai_llm_client_stub=>generate_completion( prompt = state-messages ).

        " If successful, append the response to the state messages
        state-messages = state-messages && response.

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
        ENDIF.

        " Append the human-readable error message to the state
        state-messages = state-messages && lv_error_message.

    " 4. Catch any critical, unhandled ABAP runtime errors
    CATCH cx_root INTO DATA(lx_abap_error).
        state-status = zif_ai_types=>gc_workflow_status_error.
        DATA(lv_abap_error_message) = lx_abap_error->get_text( ).
        state-messages = state-messages && |CRITICAL UNHANDLED ERROR: { lv_abap_error_message }|.

    ENDTRY.
  ENDMETHOD.
ENDCLASS.
