INTERFACE zif_ai_hitl_wait_strategy
  PUBLIC.

  METHODS handle_wait
    IMPORTING
      agent_id TYPE zif_ai_types=>ty_agent_id
    CHANGING
      state    TYPE zif_ai_types=>ts_graph_state.

ENDINTERFACE.
