CLASS zcl_ai_llm_client DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS generate_completion
      IMPORTING
        iv_system_prompt TYPE string OPTIONAL
        iv_user_prompt   TYPE string
      RETURNING
        VALUE(rv_output) TYPE string
      RAISING
        zcx_ai_agent_error.

  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_ai_llm_client IMPLEMENTATION.


  METHOD generate_completion.
    DATA lv_output TYPE string.

    TRY.
        " 1) Get API for the given ISLM scenario
        FINAL(lo_api) = cl_aic_islm_compl_api_factory=>get( )->create_instance( islm_scenario = 'ZTUM_TEST_IS03' ).

      CATCH cx_aic_api_factory INTO DATA(lx_api).
        RAISE EXCEPTION NEW zcx_ai_agent_error( error_id = zcx_ai_agent_error=>api_error
                                                severity = zif_ai_types=>gc_severity_fatal
                                                message  = lx_api->get_longtext( ) ).
    ENDTRY.

    TRY.
*        " 2) Build message container
*        FINAL(lo_msg) = lo_api->create_message_container( ).
*
*        IF iv_system_prompt IS NOT INITIAL.
*          lo_msg->set_system_role( iv_system_prompt ).
*        ENDIF.
*
*        lo_msg->add_user_message( iv_user_prompt ).
*
*
*        " 3) Execute & get completion
*        lv_output = lo_api->execute_for_messages( lo_msg )->get_completion( ).

        DATA(full_prompt) = | { iv_system_prompt }\n\n{ iv_user_prompt } |.
        lv_output = lo_api->execute_for_string( full_prompt )->get_completion( ).
      CATCH cx_aic_completion_api INTO DATA(lx_completion).
        RAISE EXCEPTION NEW zcx_ai_agent_error( error_id = zcx_ai_agent_error=>api_error
                                                severity = zif_ai_types=>gc_severity_fatal
                                                message  = lx_completion->get_longtext( ) ).
    ENDTRY.

    rv_output = lv_output.

  ENDMETHOD.

ENDCLASS.


