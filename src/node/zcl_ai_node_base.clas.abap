CLASS zcl_ai_node_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    "Read-only attributes
    DATA node_id TYPE zif_ai_types=>ty_node_id READ-ONLY.
    DATA agent_id TYPE zif_ai_types=>ty_agent_id READ-ONLY.
    " TODO: Add attributes for tools

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
ENDCLASS.
