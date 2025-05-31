# pms-application-service

`pty-mcp-server` is a multi-threaded application designed to handle multiple domain-specific tasks concurrently.  
`pms-application-service` is one of its internal packages, responsible for coordinating core application logic and managing concurrent execution across threads.

This package interprets user-intent commands (parsed from JSON-RPC requests) and executes them within specific domain contexts.  
It bridges structured input from the UI layer with the infrastructure layer's effectful operations, ensuring that requests are processed in a deterministic and decoupled manner.

In addition to command dispatching, `pms-application-service` manages the lifecycle of domain-aware applications and supervises their execution threads.  
It provides the orchestration required to safely run multiple applications in parallel, maintaining responsiveness and isolation across concurrent tasks.

---

## Package Structure

---

## Module Structure

---
