CLASS zcl_ai_prompt_library DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_prompt_template
      IMPORTING
        node_id       TYPE sysuuid_x16
        role          TYPE zai_node_prompt-role DEFAULT 'SYSTEM'
      RETURNING
        VALUE(prompt) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_ai_prompt_library IMPLEMENTATION.

  METHOD get_prompt_template.
    SELECT SINGLE prompt_id
      FROM zai_node_prompt
      WHERE node_id = @node_id
        AND role    = @role
      INTO @DATA(prompt_id).

    IF sy-subrc <> 0.
      prompt = |[Error: No prompt configured for Node { node_id } with Role { role }]|.
      RETURN.
    ENDIF.

    SELECT SINGLE template_text
      FROM zai_prompt_tpl
      WHERE prompt_id = @prompt_id
      INTO @prompt.

  ENDMETHOD.
ENDCLASS.
