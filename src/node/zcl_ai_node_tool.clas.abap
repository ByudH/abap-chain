CLASS zcl_ai_node_tool DEFINITION
  PUBLIC
  INHERITING FROM zcl_ai_node_base
  FINAL
  CREATE PUBLIC
  GLOBAL FRIENDS zcl_ai_agent_builder.

  PUBLIC SECTION.
    INTERFACES zif_ai_node_tool_aware.

    METHODS constructor
      IMPORTING
        node_id       TYPE zif_ai_types=>ty_node_id
        agent_id      TYPE zif_ai_types=>ty_agent_id
        name          TYPE string
        max_retries   TYPE i DEFAULT 2
        retry_delay_s TYPE i DEFAULT 0.

    METHODS zif_ai_node~get_configuration REDEFINITION.
    METHODS zif_ai_node~set_configuration REDEFINITION.

  PROTECTED SECTION.
*    DATA name          TYPE string.
    DATA tools         TYPE zif_ai_types=>th_tool_registry_map.
    DATA max_retries   TYPE i.
    DATA retry_delay_s TYPE i.

    METHODS do_execute REDEFINITION.

  PRIVATE SECTION.
    METHODS execute_tool_with_retry
      IMPORTING
        tool_entry    TYPE zif_ai_types=>ts_tool_registry
        input         TYPE string
      RETURNING
        VALUE(output) TYPE string
      RAISING
        cx_root.
    TYPES: BEGIN OF ts_tool_node_config,
             name          TYPE string,
             tools         TYPE zif_ai_types=>tt_tool_blueprints,
             max_retries   TYPE i,
             retry_delay_s TYPE i,
           END OF ts_tool_node_config.
ENDCLASS.


CLASS zcl_ai_node_tool IMPLEMENTATION.

  METHOD constructor.
    super->constructor( node_id = node_id agent_id = agent_id node_name = name ).
*    me->name = name.
    me->max_retries = max_retries.
    me->retry_delay_s = retry_delay_s.
  ENDMETHOD.

  METHOD zif_ai_node_tool_aware~add_tool.
    DATA entry TYPE zif_ai_types=>ts_tool_registry.
    entry-tool_name        = tool_name.
    entry-tool_endpoint    = tool.
    entry-tool_description = description.
    INSERT entry INTO TABLE tools.
  ENDMETHOD.

  METHOD zif_ai_node_tool_aware~get_tools.
    tools = tools.
  ENDMETHOD.

  METHOD zif_ai_node_tool_aware~get_tool.
    READ TABLE tools INTO DATA(entry) WITH KEY tool_name = tool_name.
    IF sy-subrc = 0.
      tool = entry-tool_endpoint.
    ENDIF.
  ENDMETHOD.

  METHOD execute_tool_with_retry.
    DATA(logger) = zcl_abapchain_logger=>get_instance( ).

    DATA attempt TYPE i VALUE 0.
    DATA last_ex TYPE REF TO cx_root.

    WHILE attempt <= max_retries.
      attempt += 1.

      TRY.
          TRY.
              logger->log_tool(
                node_id   = me->node_id
                node_name = node_name
                tool_name = tool_entry-tool_name
                message   = |Attempt { attempt }/{ max_retries + 1 } start.|
                severity  = if_bali_constants=>c_severity_information ).
            CATCH cx_root.
          ENDTRY.

          tool_entry-tool_endpoint->execute(
            EXPORTING
              input  = input
            IMPORTING
              output = output ).

          TRY.
              logger->log_tool(
                node_id   = me->node_id
                node_name = node_name
                tool_name = tool_entry-tool_name
                message   = |Attempt { attempt } success.|
                severity  = if_bali_constants=>c_severity_status ).
            CATCH cx_root.
          ENDTRY.

          RETURN.

        CATCH cx_root INTO last_ex.
          TRY.
              logger->log_tool(
                node_id   = me->node_id
                node_name = node_name
                tool_name = tool_entry-tool_name
                message   = |Attempt { attempt } failed: { last_ex->get_text( ) }|
                severity  = if_bali_constants=>c_severity_warning ).
            CATCH cx_root.
          ENDTRY.

          IF attempt > max_retries.
            RAISE EXCEPTION last_ex.
          ENDIF.

          " Optional delay between retries (0 = no delay)
          IF retry_delay_s > 0.
            WAIT UP TO retry_delay_s SECONDS.
          ENDIF.
      ENDTRY.
    ENDWHILE.
  ENDMETHOD.


  METHOD do_execute.
    DATA(logger) = zcl_abapchain_logger=>get_instance( ).

    " Expect: state-last_tool_name contains requested tool
    IF state-last_tool_name IS INITIAL.
      APPEND VALUE #( role = zif_ai_types=>gc_role_error content = |[ToolNode] No tool requested.| ) TO state-messages.
      state-branch_label = 'END'.
      RETURN.
    ENDIF.

    READ TABLE tools INTO DATA(tool_entry) WITH KEY tool_name = state-last_tool_name.
    IF sy-subrc <> 0 OR tool_entry-tool_endpoint IS INITIAL.
      APPEND VALUE #( role = zif_ai_types=>gc_role_error content = |[ToolNode] Tool not found: { state-last_tool_name }| ) TO state-messages.
      state-branch_label = 'END'.
      RETURN.
    ENDIF.

    TRY.
        logger->log_node(
          node_id  = me->node_id
          message  = |Executing tool "{ tool_entry-tool_name }" with retry.|
          severity = if_bali_constants=>c_severity_status ).
      CATCH cx_root.
    ENDTRY.

    DATA tool_output TYPE string.

    TRY.
        tool_output = execute_tool_with_retry(
          tool_entry = tool_entry
          input      = zcl_ai_utils=>messages_to_string( state-messages ) ).
      CATCH cx_root INTO DATA(ex).
        TRY.
            logger->log_error(
              message = |ToolNode failed after retries. tool="{ tool_entry-tool_name }": { ex->get_text( ) }| ).
          CATCH cx_root.
        ENDTRY.

        APPEND VALUE #( role = zif_ai_types=>gc_role_error content = |[ToolNode] Tool failed: { ex->get_text( ) }|
        ) TO state-messages.

        state-branch_label = 'END'.
        RETURN.
    ENDTRY.

    " Append tool output
    APPEND VALUE #(
    role    = zif_ai_types=>gc_role_tool
    content = |[Tool { tool_entry-tool_name } Output]| &&
    cl_abap_char_utilities=>newline &&
    tool_output
    ) TO state-messages.

    state-result_json    = tool_output.
    state-branch_label   = ''.               "'LLM' go back to LLM node
    state-last_tool_name = tool_entry-tool_name.

  ENDMETHOD.

  METHOD zif_ai_node~get_configuration.
    DATA tool_node_config TYPE ts_tool_node_config.
    tool_node_config-name          = node_name.
    tool_node_config-max_retries   = max_retries.
    tool_node_config-retry_delay_s = retry_delay_s.
    LOOP AT tools INTO DATA(tool_entry).
      DATA tool_blueprint TYPE zif_ai_types=>ts_tool_blueprint.
      tool_blueprint-tool_name        = tool_entry-tool_name.
      tool_blueprint-tool_description = tool_entry-tool_description.
      tool_blueprint-tool_class = tool_entry-tool_endpoint->get_tool_type( ).
      APPEND tool_blueprint TO tool_node_config-tools.
    ENDLOOP.
    configuration = xco_cp_json=>data->from_abap( tool_node_config )->to_string( ).
  ENDMETHOD.

  METHOD zif_ai_node~set_configuration.
    DATA tool_node_config TYPE ts_tool_node_config.
    xco_cp_json=>data->from_string( configuration )->write_to( REF #( tool_node_config ) ).
    " For data consistency, leave assigning tools for agent.
    node_name          = tool_node_config-name.
    max_retries   = tool_node_config-max_retries.
    retry_delay_s = tool_node_config-retry_delay_s.
  ENDMETHOD.

ENDCLASS.
