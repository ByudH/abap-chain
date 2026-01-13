@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'HITL Requests'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZC_AI_HITL_REQ as select from zai_hitl_req
{
    key correlation_id as CorrelationId,
    agent_id as AgentId,
    node_id as NodeId,
    topic as Topic,
    status as Status,
    reason as Reason,
    prompt as Prompt,
    response_schema as ResponseSchema,
    primary_result_field as PrimaryResultField,
    response_payload as ResponsePayload,
    created_at as CreatedAt,
    created_by as CreatedBy,
    responded_at as RespondedAt,
    responded_by as RespondedBy
}
