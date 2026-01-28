CLASS zcl_ai_node_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC
  GLOBAL FRIENDS zcl_ai_agent_builder. " To allow the agent builder to modify agent_id

  PUBLIC SECTION.
    "Read-only attributes
    DATA node_id TYPE zif_ai_types=>ty_node_id READ-ONLY. "Needed depends on the edge structure
    DATA agent_id TYPE zif_ai_types=>ty_agent_id READ-ONLY.
    DATA node_name TYPE string READ-ONLY.
    " TODO: Think about the shared functionality for all nodes, e.g. logging, error handling, etc.

    INTERFACES zif_ai_node.
    "Declare constructor
    METHODS constructor
      IMPORTING
        node_name TYPE string.
  PROTECTED SECTION.
    METHODS do_execute
      CHANGING
        state TYPE zif_ai_types=>ts_graph_state.
    "logging
    METHODS log_message
      IMPORTING
        message  TYPE string
        severity TYPE if_bali_constants=>ty_severity DEFAULT if_bali_constants=>c_severity_information.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ai_node_base IMPLEMENTATION.
  METHOD constructor.
    " the builder will set the agent_id later since when generating the node we might not know the agent yet
    me->node_name = node_name.
    me->node_id = zcl_ai_utils=>generate_uuid( ).
  ENDMETHOD.

  METHOD zif_ai_node~execute.

  " TODO: Logging


    do_execute(
      CHANGING
        state = state
    ).

  ENDMETHOD.

  METHOD do_execute.
    DATA(new_message) = VALUE zif_ai_types=>ts_message(
    role    = zif_ai_types=>gc_role_assistant
    content = |Base Node { me->node_id } of agent { me->agent_id } executed.|
    ).

    APPEND new_message TO state-messages.
  ENDMETHOD.

  METHOD zif_ai_node~get_node_id.
    node_id = me->node_id.
  ENDMETHOD.

  METHOD log_message.
    DATA(logger) = zcl_abapchain_logger=>get_instance( ).
    logger->log_node_ref(
      node     = me
      message  = message
      severity = severity ).
  ENDMETHOD.

  METHOD zif_ai_node~get_node_name.
    node_name = me->node_name.
  ENDMETHOD.

  METHOD zif_ai_node~get_node_type.
    DATA(type_descr) = cl_abap_typedescr=>describe_by_object_ref( me ).
    node_type = type_descr->get_relative_name( ).
  ENDMETHOD.
ENDCLASS.
