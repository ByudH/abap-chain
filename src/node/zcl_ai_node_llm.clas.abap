CLASS zcl_ai_node_llm DEFINITION
  PUBLIC
  INHERITING FROM zcl_ai_node_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_ai_node~execute
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ai_node_llm IMPLEMENTATION.
  METHOD zif_ai_node~execute.
    " The initial implementaiton is to use the LLM client stub we created.
    DATA(response) = NEW zcl_ai_llm_client_stub( )->generate_completion( prompt = state_input-messages ).
    state_output = state_input.
    state_output-messages = state_input-messages && response.
  ENDMETHOD.
ENDCLASS.
