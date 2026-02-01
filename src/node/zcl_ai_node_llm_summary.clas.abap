CLASS zcl_ai_node_llm_summary DEFINITION
  PUBLIC
  INHERITING FROM zcl_ai_node_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        name        TYPE string DEFAULT 'Summary'
        user_prompt TYPE string DEFAULT

'Create a concise summary of the conversation so far. Include: what was assessed, key findings, approvals/decisions, external enrichments if present. Keep it short.'.

    METHODS zif_ai_node~get_configuration REDEFINITION.
    METHODS zif_ai_node~set_configuration REDEFINITION.

    METHODS set_user_prompt
      IMPORTING
        user_prompt TYPE string.

    DATA user_prompt TYPE string READ-ONLY.

  PROTECTED SECTION.
    METHODS do_execute REDEFINITION.

  PRIVATE SECTION.
    DATA mo_llm_client TYPE REF TO zcl_ai_llm_client.

    TYPES: BEGIN OF ts_summary_node_config,
             name        TYPE string,
             user_prompt TYPE string,
           END OF ts_summary_node_config.

ENDCLASS.



CLASS zcl_ai_node_llm_summary IMPLEMENTATION.

  METHOD constructor.
    super->constructor( node_name = name ).
    mo_llm_client   = NEW zcl_ai_llm_client( ).
    me->user_prompt = user_prompt.
  ENDMETHOD.


  METHOD do_execute.
    " Summary node is PURE:
    " - It never drives control flow
    " - It never sets status/branch/tool fields
    " - It only appends one assistant message (the summary)

    DATA(lv_system_prompt) = zcl_ai_utils=>messages_to_string( state-messages ).
    DATA(lv_user_prompt)   = me->user_prompt.

    TRY.
        DATA(lv_llm_output) = mo_llm_client->generate_completion(
          iv_system_prompt = lv_system_prompt
          iv_user_prompt   = lv_user_prompt ).

        " Keep your schema validator so the LLM output stays structured,
        " but we only USE final_answer here.
        DATA(ls_resp) = zcl_llm_response_validator=>parse_and_validate( lv_llm_output ).

        DATA lv_summary TYPE string.

        IF ls_resp-final_answer IS NOT INITIAL.
          lv_summary = ls_resp-final_answer.
        ELSEIF ls_resp-reasoning IS NOT INITIAL.
          " Fallback (some models may forget final_answer)
          lv_summary = ls_resp-reasoning.
        ELSE.
          lv_summary = 'Summary could not be generated (empty LLM response).'.
        ENDIF.

        APPEND VALUE #( role = zif_ai_types=>gc_role_assistant content = lv_summary )
          TO state-messages.

      CATCH zcx_ai_agent_error INTO DATA(lx_error).
        " Do NOT set END / branch / status here.
        " Just record the error in messages; orchestrator decides what happens.
        APPEND VALUE #(
          role    = zif_ai_types=>gc_role_error
          content = |Summary node LLM error: { lx_error->message }| )
          TO state-messages.

      CATCH cx_root INTO DATA(lx_abap_error).
        APPEND VALUE #(
          role    = zif_ai_types=>gc_role_error
          content = |Summary node runtime error: { lx_abap_error->get_text( ) }| )
          TO state-messages.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_ai_node~get_configuration.
    DATA ls_cfg TYPE ts_summary_node_config.
    ls_cfg-name        = node_name.
    ls_cfg-user_prompt = me->user_prompt.
    configuration = xco_cp_json=>data->from_abap( ls_cfg )->to_string( ).
  ENDMETHOD.


  METHOD zif_ai_node~set_configuration.
    DATA ls_cfg TYPE ts_summary_node_config.
    xco_cp_json=>data->from_string( configuration )->write_to( REF #( ls_cfg ) ).
    node_name       = ls_cfg-name.
    me->user_prompt = ls_cfg-user_prompt.
  ENDMETHOD.


  METHOD set_user_prompt.
    me->user_prompt = user_prompt.
  ENDMETHOD.

ENDCLASS.
