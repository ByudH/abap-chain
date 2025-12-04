INTERFACE zif_ai_node
  PUBLIC .
  METHODS execute
    IMPORTING
      state_input         TYPE zif_ai_types=>ty_graph_state
    RETURNING
      VALUE(state_output) TYPE zif_ai_types=>ty_graph_state.
ENDINTERFACE.
