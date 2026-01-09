CLASS zcl_ai_agent_repository DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS save_agent_blueprint
      IMPORTING
                agent_blueprint TYPE zif_ai_types=>ts_agent_blueprint
      RAISING   cx_root.
    CLASS-METHODS load_agent_blueprint
      IMPORTING
                agent_id               TYPE string
      RETURNING
                VALUE(agent_blueprint) TYPE zif_ai_types=>ts_agent_blueprint
      RAISING   cx_root.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ai_agent_repository IMPLEMENTATION.

  METHOD save_agent_blueprint.
    DATA agent_def TYPE zai_agent_def.
    agent_def-agent_id = agent_blueprint-agent_id.
    agent_def-agent_name = agent_blueprint-agent_name.
    agent_def-start_node_id = agent_blueprint-start_node_id.
    agent_def-graph_json = xco_cp_json=>data->from_abap( agent_blueprint-graph_blueprint )->to_string( ).
    agent_def-tool_registry_json = xco_cp_json=>data->from_abap( agent_blueprint-tool_registry_blueprint )->to_string( ).

    MODIFY zai_agent_def FROM @agent_def.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_open_sql_db. " Or your custom exception
    ENDIF.
  ENDMETHOD.

  METHOD load_agent_blueprint.
    SELECT SINGLE *
    FROM zai_agent_def
    WHERE agent_id = @agent_id
    INTO @DATA(agent_def).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_open_sql_db. " Or your custom exception
    ENDIF.
    agent_blueprint-agent_name = agent_def-agent_name.
    agent_blueprint-start_node_id = agent_def-start_node_id.
    xco_cp_json=>data->from_string( agent_def-graph_json )->write_to( REF #( agent_blueprint-graph_blueprint ) ).
    xco_cp_json=>data->from_string( agent_def-tool_registry_json )->write_to( REF #( agent_blueprint-tool_registry_blueprint ) ).
  ENDMETHOD.
ENDCLASS.
