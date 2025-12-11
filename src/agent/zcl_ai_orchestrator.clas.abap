CLASS zcl_ai_orchestrator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
*        it_nodes         TYPE zif_ai_types=>tt_node_registry_lh
*        it_edges         TYPE zif_ai_types=>tt_edge_lh
        node_edge_graph    TYPE zif_ai_types=>th_graph_map
        start_node_id      TYPE zif_ai_types=>ty_node_id
        initial_state      TYPE zif_ai_types=>ts_graph_state OPTIONAL
      RETURNING
        VALUE(final_state) TYPE zif_ai_types=>ts_graph_state.

ENDCLASS.



CLASS zcl_ai_orchestrator IMPLEMENTATION.

  METHOD run.
    DATA state TYPE zif_ai_types=>ts_graph_state.
    state = initial_state.

    DATA current_node_id TYPE zif_ai_types=>ty_node_id.
    current_node_id = start_node_id.

    IF current_node_id IS INITIAL.
      " No start node -> nothing to do
      final_state = state.
      RETURN.
    ENDIF.

    DATA graph_entry      TYPE zif_ai_types=>ts_graph_entry.
    DATA edge            TYPE zif_ai_types=>ts_edge.
    DATA edges_for_node  TYPE zif_ai_types=>tt_edge_list.
    DATA next_node_id    TYPE zif_ai_types=>ty_node_id.

    " Optional: avoid infinite loops in bad graphs
    DATA step_count TYPE i VALUE 0.
    CONSTANTS max_steps TYPE i VALUE 50.

    WHILE current_node_id IS NOT INITIAL.

      step_count = step_count + 1.
      IF step_count > max_steps.
        " Safety break: prevent endless loops in broken graphs
        EXIT.
      ENDIF.


      " 1) Find current node in graph map (hashed table)
      CLEAR graph_entry.
      READ TABLE node_edge_graph INTO graph_entry
           WITH KEY source_node_id = current_node_id.

      IF sy-subrc <> 0 OR graph_entry-source_node IS INITIAL.
        " Inconsistent graph: node id not found → stop
        EXIT.
      ENDIF.


      " 2) Execute node (polymorphic via zif_ai_node)
*      state = graph_entry-source_node->execute(
*                   state_input = ls_state ).

      graph_entry-source_node->execute(
        CHANGING
            state = state ).


      " 3) Collect outgoing edges for this node
      CLEAR edges_for_node.
      edges_for_node = graph_entry-next_nodes.

      IF edges_for_node IS INITIAL.
        " No outgoing edges → implicit END
        EXIT.
      ENDIF.

      " Sort edges by priority (lower = higher priority)
      SORT edges_for_node BY priority ASCENDING.

      " 4) Determine next node:
      "    a) ON_CONTROL: condition_value = state.branch_label
      "    b) ALWAYS: unconditional fallback
      CLEAR next_node_id.

      " First: ON_CONTROL
      LOOP AT edges_for_node INTO edge
           WHERE condition       = zif_ai_types=>gc_cond_on_control
             AND condition_value = state-branch_label.
        next_node_id = edge-target_node_id.
        EXIT.
      ENDLOOP.

      " If no ON_CONTROL matched, try ALWAYS
      IF next_node_id IS INITIAL.
        LOOP AT edges_for_node INTO edge
             WHERE condition = zif_ai_types=>gc_cond_always.
          next_node_id = edge-target_node_id.
          EXIT.
        ENDLOOP.
      ENDIF.

      " No matching edge -> terminate
      IF next_node_id IS INITIAL.
        EXIT.
      ENDIF.

      " Move to next node
      current_node_id = next_node_id.

    ENDWHILE.

    final_state = state.

  ENDMETHOD.

ENDCLASS.
