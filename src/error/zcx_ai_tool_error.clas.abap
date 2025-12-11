CLASS zcx_ai_tool_error DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        error_message TYPE string.
    METHODS get_text REDEFINITION.
  PROTECTED SECTION.
    DATA error_message TYPE string.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_ai_tool_error IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( ).
    me->error_message = error_message.
  ENDMETHOD.
  METHOD get_text.
*      cl_message_helper=>get_text_for_message(
*        EXPORTING
*          text = me
*        RECEIVING
*          result = result
*      ).
    result = me->error_message.
  ENDMETHOD.
ENDCLASS.
