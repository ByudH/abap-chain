INTERFACE zif_ai_node
  PUBLIC .
  methods execute
    importing
      state_input type ref to ZCL_AI_GRAPH_STATE
    returning
      value(state_output) type ref to ZCL_AI_GRAPH_STATE.
ENDINTERFACE.
