CLASS zcl_ai_node_base_lh DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA node_id  TYPE zif_ai_types=>ty_node_id READ-ONLY.
    DATA agent_id TYPE zif_ai_types=>ty_agent_id READ-ONLY.

    METHODS constructor
      IMPORTING
        node_id  TYPE zif_ai_types=>ty_node_id
        agent_id TYPE zif_ai_types=>ty_agent_id.

    INTERFACES zif_ai_node.

  PROTECTED SECTION.

    " Hook for concrete nodes – must be implemented by subclasses
    METHODS do_execute
      IMPORTING
        state_input         TYPE zif_ai_types=>ty_graph_state
      RETURNING
        VALUE(state_output) TYPE zif_ai_types=>ty_graph_state.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_ai_node_base_lh IMPLEMENTATION.


  METHOD constructor.
    me->node_id  = node_id.
    me->agent_id = agent_id.
  ENDMETHOD.

  METHOD zif_ai_node~execute.
    " Central entry point used by orchestrator
    " Delegates to subclass-specific core implementation
    state_output = me->do_execute( state_input ).
  ENDMETHOD.

  METHOD do_execute.
    " not implemented
  ENDMETHOD.

ENDCLASS.
