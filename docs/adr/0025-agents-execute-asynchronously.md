# Agents execute asynchronously

Status: accepted

Mevedel will have one asynchronous execution model for all agent turns rather than foreground and background agent variants. A workflow such as `/review`, `/verify`, or a fork skill may keep its own interaction open while awaiting an agent result, but that is a property of the owning workflow rather than a different agent runtime. This preserves synchronous user-facing workflows while removing duplicate dispatch, settlement, watchdog, and result-delivery paths.
