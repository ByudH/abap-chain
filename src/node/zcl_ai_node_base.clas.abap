CLASS zcl_ai_node_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    "Read-only attributes
    DATA node_id TYPE zif_ai_types=>ty_node_id READ-ONLY. "Needed depends on the edge structure
    DATA agent_id TYPE zif_ai_types=>ty_agent_id READ-ONLY.
    " TODO: Think about the shared functionality for all nodes, e.g. logging, error handling, etc.

    "Declare constructor
    METHODS constructor
      IMPORTING
        node_id  TYPE zif_ai_types=>ty_node_id
        agent_id TYPE zif_ai_types=>ty_agent_id.
    INTERFACES zif_ai_node.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_ai_node_base IMPLEMENTATION.
  METHOD constructor.
    me->node_id = node_id.
    me->agent_id = agent_id.
  ENDMETHOD.
  METHOD zif_ai_node~execute.
    state_output = state_input.
    state_output-messages = state_input-messages && |Base Node { me->node_id } of agent { me->agent_id } executed. No concrete implementation.|.
  ENDMETHOD.
  METHOD zif_ai_node~get_node_id.
    node_id = me->node_id.
  ENDMETHOD.
ENDCLASS.
