@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'HITL Requests (Projection)'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZC_AI_HITL_REQ_P as projection on ZC_AI_HITL_REQ
{
    key CorrelationId,
    AgentId,
    NodeId,
    Topic,
    Status,
    Reason,
    Prompt,
    ResponseSchema,
    PrimaryResultField,
    ResponsePayload,
    CreatedAt,
    CreatedBy,
    RespondedAt,
    RespondedBy
}
