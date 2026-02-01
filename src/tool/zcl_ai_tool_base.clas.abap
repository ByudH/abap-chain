CLASS zcl_ai_tool_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_ai_tool.

    METHODS constructor
      IMPORTING
        name        TYPE string
        description TYPE string.


  PROTECTED SECTION.
    METHODS:

      do_execute
        IMPORTING
                  input  TYPE string
        EXPORTING
                  output TYPE string
        RAISING   cx_root.

    METHODS define_argument_metadata
      RETURNING VALUE(arguments) TYPE zcl_tool_schema=>tt_tool_arguments.

    DATA name TYPE string.
    DATA description TYPE string.

  PRIVATE SECTION.
    TYPES: BEGIN OF ts_tool_config,
             name        TYPE string,
             description TYPE string,
           END OF ts_tool_config.
ENDCLASS.

CLASS zcl_ai_tool_base IMPLEMENTATION.

  METHOD constructor.
    me->name = name.
    me->description = description.
  ENDMETHOD.

  METHOD zif_ai_tool~get_name.
    name = me->name.
  ENDMETHOD.

  METHOD zif_ai_tool~get_description.
    description = me->description.
  ENDMETHOD.

  METHOD zif_ai_tool~get_tool_type.
    DATA(type_descr) = cl_abap_typedescr=>describe_by_object_ref( me ).
    tool_type = type_descr->get_relative_name( ).
  ENDMETHOD.

  METHOD do_execute.
    " ------------------------------------------------------------
    " Default implementation – concrete tools should redefine this
    " ------------------------------------------------------------
    output = |[Tool { me->name } has no implementation yet].|.
  ENDMETHOD.

  METHOD zif_ai_tool~execute.
    DATA(logger) = zcl_abapchain_logger=>get_instance( ).

    logger->log_tool_ctx(
      node     = calling_node
      tool     = me
      message  = |execute start. input_len={ strlen( input ) }.|
      severity = if_bali_constants=>c_severity_information ).

    TRY.
        do_execute( EXPORTING input = input IMPORTING output = output ).
        status = 0.
      CATCH cx_root INTO DATA(ex).
        status = 1.
        logger->log_tool_ctx(
          node     = calling_node
          tool     = me
          message  = |execute FAILED: { ex->get_text( ) }|
          severity = if_bali_constants=>c_severity_error ).

        RAISE EXCEPTION NEW zcx_ai_tool_error( error_message = |Tool "{ me->name }" failed: { ex->get_text( ) }| ).

    ENDTRY.

    logger->log_tool_ctx(
      node     = calling_node
      tool     = me
      message  = |execute end. output_len={ strlen( output ) }.|
      severity = if_bali_constants=>c_severity_information ).
  ENDMETHOD.


  METHOD zif_ai_tool~get_argument_metadata.
    " Delegate to protected method that subclasses can override
    arguments = define_argument_metadata( ).
  ENDMETHOD.

  METHOD define_argument_metadata.
    " Default: no arguments
    " Subclasses override this to define their arguments
    CLEAR arguments.
  ENDMETHOD.

  METHOD zif_ai_tool~get_configuration.
    DATA config TYPE ts_tool_config.

    config-name = me->name.
    config-description = me->description.

    configuration = xco_cp_json=>data->from_abap( config )->to_string( ).
  ENDMETHOD.

  METHOD zif_ai_tool~set_configuration.
    DATA config TYPE ts_tool_config.

    xco_cp_json=>data->from_string( configuration )->write_to( REF #( config ) ).

    me->name = config-name.
    me->description = config-description.
  ENDMETHOD.

ENDCLASS.
