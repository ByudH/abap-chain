CLASS zcl_ai_tool_fake_risk_check DEFINITION
  PUBLIC
    INHERITING FROM zcl_ai_tool_base
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        demo_risk_level TYPE string DEFAULT 'LOW'.

    METHODS zif_ai_tool~get_argument_metadata REDEFINITION.

  PROTECTED SECTION.
    METHODS do_execute REDEFINITION.

  PRIVATE SECTION.
    DATA demo_risk_level TYPE string.
ENDCLASS.



CLASS zcl_ai_tool_fake_risk_check IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
      name        = 'FAKE_RISK_CHECK'
      description = 'Performs a mock risk assessment and returns a LOW/MEDIUM/HIGH risk label.'
    ).

    " Normalize input
    me->demo_risk_level = demo_risk_level.
    TRANSLATE me->demo_risk_level TO UPPER CASE.

    IF me->demo_risk_level <> 'LOW'
       AND me->demo_risk_level <> 'MEDIUM'
       AND me->demo_risk_level <> 'HIGH'.
      me->demo_risk_level = 'LOW'.
    ENDIF.
  ENDMETHOD.


  METHOD do_execute.
    CASE demo_risk_level.
      WHEN 'HIGH'.
        output =
          |[Tool { name }]: Mock risk check: Risk level HIGH, critical findings detected.|.

      WHEN 'MEDIUM'.
        output =
          |[Tool { name }]: Mock risk check: Risk level MEDIUM, some findings require attention.|.

      WHEN OTHERS.
        output =
          |[Tool { name }]: Mock risk check: Risk level LOW, 0 critical findings.|.

    ENDCASE.
  ENDMETHOD.


  METHOD zif_ai_tool~get_argument_metadata.
    " No runtime arguments – demo-controlled via constructor
  ENDMETHOD.

ENDCLASS.
