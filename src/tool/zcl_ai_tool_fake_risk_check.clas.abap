CLASS zcl_ai_tool_fake_risk_check DEFINITION
  PUBLIC
    INHERITING FROM zcl_ai_tool_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        iv_name TYPE string.

  PROTECTED SECTION.
    METHODS do_execute REDEFINITION.


  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ai_tool_fake_risk_check IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
        iv_name        = 'FAKE_TABLE_INFO'
        iv_description = 'Returns mock metadata about an ABAP table (fields, rows, basic info).'
        ).
  ENDMETHOD.

  METHOD do_execute.
    ev_output = |[Tool { mv_name }]: Mock risk check: Risk level LOW, no critical findings.| .
  ENDMETHOD.
ENDCLASS.
