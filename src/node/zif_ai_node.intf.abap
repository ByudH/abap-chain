INTERFACE zif_ai_node
  PUBLIC .
  METHODS execute
    IMPORTING
      state_input         TYPE REF TO zif_ai_types=>ty_graph_state
    RETURNING
      VALUE(state_output) TYPE REF TO zif_ai_types=>ty_graph_state.
ENDINTERFACE.
