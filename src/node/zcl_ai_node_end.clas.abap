CLASS zcl_ai_node_end DEFINITION
  PUBLIC
  INHERITING FROM zcl_ai_node_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  METHODS constructor
      IMPORTING
        node_id  TYPE zif_ai_types=>ty_node_id
        agent_id TYPE zif_ai_types=>ty_agent_id.

    METHODS zif_ai_node~execute
    REDEFINITION.
    PROTECTED SECTION.
    PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ai_node_end IMPLEMENTATION.
METHOD constructor.
    " Just call the constructor of the upper class
    super->constructor(
      node_id  = node_id
      agent_id = agent_id
    ).
  ENDMETHOD.

  METHOD zif_ai_node~execute.
    " END node is basically a no operation will run further:
    " just forward the state and add an optional log message.
    state_output = state_input.

    DATA(lv_msg) =
      |END node { me->node_id } of agent { me->agent_id } reached. Execution will stop.|.

    state_output-messages = state_input-messages && lv_msg.
  ENDMETHOD.

ENDCLASS.
