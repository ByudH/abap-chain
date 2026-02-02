INTERFACE zif_ai_planner_types PUBLIC.

  TYPES: BEGIN OF ts_hitl,
           topic           TYPE string,
           reason          TYPE string,
           prompt          TYPE string,
           primary_field   TYPE string,
           response_schema TYPE string,
         END OF ts_hitl.

  "Raw type: tool_arguments can be ANY (data ref)
  TYPES: BEGIN OF ts_planner_decision_raw,
           decision          TYPE string,
           branch_label      TYPE string,
           assistant_message TYPE string,
           tool_name         TYPE string,
           tool_arguments    TYPE REF TO data, " <-- key point
           hitl              TYPE ts_hitl,
         END OF ts_planner_decision_raw.

  "Final type: tool_arguments stored as JSON string
  TYPES: BEGIN OF ts_planner_decision,
           decision          TYPE string,
           branch_label      TYPE string,
           assistant_message TYPE string,
           tool_name         TYPE string,
           tool_arguments    TYPE string,
           hitl              TYPE ts_hitl,
         END OF ts_planner_decision.

ENDINTERFACE.
