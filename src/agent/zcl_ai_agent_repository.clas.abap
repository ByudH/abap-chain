CLASS zcl_ai_agent_repository DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS save_agent_blueprint
      IMPORTING
        agent_id                TYPE string
        start_node_id           TYPE string
        agent_name              TYPE string
        graph_blueprint         TYPE zif_ai_types=>tt_graph_blueprint
        tool_registry_blueprint TYPE zif_ai_types=>tt_tool_blueprints.
    CLASS-METHODS load_agent_blueprint
      IMPORTING
        agent_id                TYPE string
      EXPORTING
        agent_name              TYPE string
        start_node_id           TYPE string
        graph_blueprint         TYPE zif_ai_types=>tt_graph_blueprint
        tool_registry_blueprint TYPE zif_ai_types=>tt_tool_blueprints.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ai_agent_repository IMPLEMENTATION.

  METHOD save_agent_blueprint.
    DATA agent_def TYPE zai_agent_def.
    agent_def-agent_id = agent_id.
    agent_def-agent_name = agent_name.
    agent_def-start_node_id = start_node_id.
    agent_def-graph_json = xco_cp_json=>data->from_abap( graph_blueprint )->to_string( ).
    agent_def-tool_registry_json = xco_cp_json=>data->from_abap( tool_registry_blueprint )->to_string( ).
    MODIFY zai_agent_def FROM @agent_def.
  ENDMETHOD.

  METHOD load_agent_blueprint.
  ENDMETHOD.
ENDCLASS.
