CLASS zcl_ai_node_end DEFINITION
  PUBLIC
  INHERITING FROM zcl_ai_node_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        name TYPE string default 'End_Node'.

    METHODS zif_ai_node~get_configuration REDEFINITION.
    METHODS zif_ai_node~set_configuration REDEFINITION.

  PROTECTED SECTION.
    METHODS do_execute
        REDEFINITION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ts_end_node_config,
             name TYPE string, " Placeholder for future configuration options
           END OF ts_end_node_config.
ENDCLASS.



CLASS zcl_ai_node_end IMPLEMENTATION.
  METHOD constructor.
    " Just call the constructor of the upper class
    super->constructor(
      name
      ).
  ENDMETHOD.

  METHOD do_execute.
    " END node is basically a no operation will run further:
    " just forward the state and add an optional log message.
    APPEND VALUE zif_ai_types=>ts_message( role = zif_ai_types=>gc_role_assistant content = |END node { me->node_id } of agent { me->agent_id } reached. Execution will stop.| ) TO state-messages.

  ENDMETHOD.

  METHOD zif_ai_node~get_configuration.
    DATA config TYPE ts_end_node_config.
    config-name = me->node_name.
    configuration = xco_cp_json=>data->from_abap( config )->to_string( ).
  ENDMETHOD.

  METHOD zif_ai_node~set_configuration.
    DATA config TYPE ts_end_node_config.
    xco_cp_json=>data->from_string( configuration )->write_to( REF #( config ) ).
    me->node_name = config-name.
  ENDMETHOD.

ENDCLASS.
