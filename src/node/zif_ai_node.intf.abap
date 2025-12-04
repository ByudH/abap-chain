INTERFACE zif_ai_node
  PUBLIC .
  METHODS execute
    IMPORTING
      state_input         TYPE REF TO zcl_ai_graph_state
    RETURNING
      VALUE(state_output) TYPE REF TO zcl_ai_graph_state.
ENDINTERFACE.
