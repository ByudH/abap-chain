INTERFACE zif_ai_hitl_request_service
  PUBLIC.

  METHODS request_input
    IMPORTING
      agent_id TYPE zif_ai_types=>ty_agent_id
      node_id  TYPE zif_ai_types=>ty_node_id
      state    TYPE zif_ai_types=>ts_graph_state
    RAISING
      cx_root.

ENDINTERFACE.
