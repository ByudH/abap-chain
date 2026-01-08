INTERFACE zif_ai_node
  PUBLIC .
  METHODS execute
    CHANGING state TYPE zif_ai_types=>ts_graph_state.
  " may add return value later


*    IMPORTING
*      state_input         TYPE zif_ai_types=>ts_graph_state
*    " Change the keyword to Changing
*    RETURNING
*      VALUE(state_output) TYPE zif_ai_types=>ts_graph_state.

  METHODS get_node_id
    RETURNING
      VALUE(node_id) TYPE zif_ai_types=>ty_node_id.

  METHODS get_node_type
    RETURNING
      VALUE(node_type) TYPE string.

  METHODS get_configuration
    RETURNING
      VALUE(configuration) TYPE string.

  METHODS set_configuration
    IMPORTING
      configuration TYPE string.
ENDINTERFACE.
