CLASS zcx_ai_agent_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        text TYPE string.
    DATA: text_to_show TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcx_ai_agent_error IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( textid = '000' ).
    me->text_to_show = text.
  ENDMETHOD.
ENDCLASS.



