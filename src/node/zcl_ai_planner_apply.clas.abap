CLASS zcl_ai_planner_apply DEFINITION
  PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS apply
      IMPORTING decision TYPE zif_ai_planner_types_lh=>ts_planner_decision
      CHANGING  state    TYPE zif_ai_types=>ts_graph_state
                messages TYPE zif_ai_types=>tt_messages.

ENDCLASS.

CLASS zcl_ai_planner_apply IMPLEMENTATION.

  METHOD apply.
    "Optional trace message
    IF decision-assistant_message IS NOT INITIAL.
      APPEND VALUE #(
        role    = zif_ai_types=>gc_role_assistant
        content = |[Planner] { decision-assistant_message }|
      ) TO messages.
    ENDIF.

    "Force canonical routing label
    state-branch_label = decision-decision.

    CASE decision-decision.
      WHEN 'TOOL'.
        state-last_tool_name = decision-tool_name.

        "tool_arguments: if empty, set {}
        state-tool_arguments = COND string(
          WHEN decision-tool_arguments IS INITIAL THEN '{}'
          ELSE decision-tool_arguments ).

      WHEN 'HITL'.
        state-hitl_topic           = decision-hitl-topic.
        state-hitl_reason          = decision-hitl-reason.
        state-hitl_prompt          = decision-hitl-prompt.
        state-hitl_primary_field   = decision-hitl-primary_field.
        state-hitl_response_schema = decision-hitl-response_schema.

      WHEN 'FINAL'.
        "No extra fields needed

      WHEN 'END'.
        "Optional: mark workflow error, your call:
        "state-status = zif_ai_types=>gc_workflow_status_error.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
