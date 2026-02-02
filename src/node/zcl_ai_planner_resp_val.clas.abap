CLASS zcl_ai_planner_resp_val DEFINITION
  PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS parse_and_validate
      IMPORTING json TYPE string
      RETURNING VALUE(decision) TYPE zif_ai_planner_types=>ts_planner_decision
      RAISING zcx_parse_error.

ENDCLASS.

CLASS zcl_ai_planner_resp_val IMPLEMENTATION.

  METHOD parse_and_validate.
    DATA raw TYPE zif_ai_planner_types=>ts_planner_decision_raw.

    "Deserialize with /UI2/CL_JSON (handles unknown object shapes)
    TRY.
        /ui2/cl_json=>deserialize(
          EXPORTING json = json
          CHANGING  data = raw ).
      CATCH cx_root INTO DATA(parse_error).
        RAISE EXCEPTION TYPE zcx_parse_error
          EXPORTING error_text = |Planner JSON parse failed: { parse_error->get_text( ) }|.
    ENDTRY.

    "Copy primitives
    decision-decision          = raw-decision.
    decision-branch_label      = raw-branch_label.
    decision-assistant_message = raw-assistant_message.
    decision-tool_name         = raw-tool_name.
    decision-hitl              = raw-hitl.

    "Normalize decision
    TRANSLATE decision-decision TO UPPER CASE.
    IF decision-branch_label IS INITIAL.
      decision-branch_label = decision-decision.
    ENDIF.

    "tool_arguments: turn ANY into JSON string
    IF raw-tool_arguments IS BOUND.
      TRY.
          /ui2/cl_json=>serialize(
            EXPORTING data        = raw-tool_arguments
                      pretty_name = abap_false
            RECEIVING r_json      = decision-tool_arguments ).
        CATCH cx_root.
          "Fallback
          decision-tool_arguments = '{}'.
      ENDTRY.
    ELSE.
      decision-tool_arguments = '{}'.
    ENDIF.

    "Validate decision
    IF decision-decision <> 'TOOL'
       AND decision-decision <> 'HITL'
       AND decision-decision <> 'FINAL'
       AND decision-decision <> 'END'.
      RAISE EXCEPTION TYPE zcx_parse_error
        EXPORTING error_text = |Invalid planner decision "{ decision-decision }"|.
    ENDIF.

    IF decision-decision = 'TOOL' AND decision-tool_name IS INITIAL.
      RAISE EXCEPTION TYPE zcx_parse_error
        EXPORTING error_text = |Decision TOOL but tool_name missing|.
    ENDIF.

    IF decision-decision = 'HITL'
       AND ( decision-hitl-topic IS INITIAL
          OR decision-hitl-reason IS INITIAL
          OR decision-hitl-prompt IS INITIAL
          OR decision-hitl-primary_field IS INITIAL
          OR decision-hitl-response_schema IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_parse_error
        EXPORTING error_text = |Decision HITL but hitl payload incomplete|.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
