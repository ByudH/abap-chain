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


  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ai_tool_fake_table_info IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
        name         = 'FAKE_RISK_CHECK'
        description    = 'Performs a mock risk assessment and returns a LOW/MEDIUM/HIGH risk label.'
      ).
  ENDMETHOD.

  METHOD do_execute.
    output = |[Tool { name }]: Mock risk check: Risk level LOW, no critical findings.| .
  ENDMETHOD.
ENDCLASS.
