CLASS zcl_ai_tool_fake_risk_check DEFINITION
  PUBLIC
    INHERITING FROM zcl_ai_tool_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor.
  " Since the constructor creates a tool with a fixed name and description no importing variables are needed.
*      IMPORTING
*        name TYPE string.

  PROTECTED SECTION.
    METHODS do_execute REDEFINITION.


  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ai_tool_fake_risk_check IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
        name        = 'FAKE_TABLE_INFO'
        description = 'Returns mock metadata about an ABAP table (fields, rows, basic info).'
        ).
  ENDMETHOD.

  METHOD do_execute.
    output = |[Tool { me->name }]: Mock risk check: Risk level LOW, no critical findings.| .
  ENDMETHOD.
ENDCLASS.
