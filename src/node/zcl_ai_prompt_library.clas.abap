CLASS zcl_ai_prompt_library DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_prompt_template
      IMPORTING
        iv_node_id       TYPE sysuuid_x16
        iv_role          TYPE zai_node_prompt-role DEFAULT 'SYSTEM'
      RETURNING
        VALUE(rv_prompt) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_ai_prompt_library IMPLEMENTATION.

  METHOD get_prompt_template.
    SELECT SINGLE prompt_id
      FROM zai_node_prompt
      WHERE node_id = @iv_node_id
        AND role    = @iv_role
      INTO @DATA(lv_prompt_id).

    IF sy-subrc <> 0.
      rv_prompt = |[Error: No prompt configured for Node { iv_node_id } with Role { iv_role }]|.
      RETURN.
    ENDIF.

    SELECT SINGLE template_text
      FROM zai_prompt_tpl
      WHERE prompt_id = @lv_prompt_id
      INTO @rv_prompt.

  ENDMETHOD.
ENDCLASS.
