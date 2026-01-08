CLASS zcl_ai_tool_fake_table_info DEFINITION
  PUBLIC
    INHERITING FROM zcl_ai_tool_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor.
*      IMPORTING
*        name TYPE string.

  PROTECTED SECTION.
    METHODS do_execute REDEFINITION.
    METHODS define_argument_metadata REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ai_tool_fake_table_info IMPLEMENTATION.
  METHOD constructor.
    super->constructor(
        name        = 'FAKE_TABLE_INFO'
        description = 'Returns mock metadata about an ABAP table (fields, rows, basic info).'
        ).
  ENDMETHOD.


    METHOD define_argument_metadata.
    " Mock tool with sample arguments for testing related to task 1.2.6
    arguments = VALUE #(
      ( name        = 'table_name'
        type        = 'string'
        description = 'Name of table to analyze (mock data)'
        required    = abap_true
        default     = '' )
      ( name        = 'include_sample_data'
        type        = 'boolean'
        description = 'Whether to include sample rows in response'
        required    = abap_false
        default     = 'false' )
    ).
  ENDMETHOD.

  METHOD do_execute.
    output = |[Tool { me->name }]: Mock table info: VBAK has 10 rowns.| .
  ENDMETHOD.
ENDCLASS.
