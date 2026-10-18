## Agent Skills (SkillsJars)

This project declares Agent Skills as build-only dependencies in the sbt plugin's `Skills` configuration. Before working, extract them with:

```bash
./sbta extractSkillsJars
```

The generated skills are written under `.kiro/skills/`. Read relevant `SKILL.md` files there and follow their guidance. The generated directory is ignored because it can be reproduced from the pinned dependency in `build.sbt`.

## Agent tooling

- Follow the `zen-of-projects` Skill (extract with `./sbta extractSkillsJars` into the gitignored `.kiro/skills/`); this file records only project-specific facts and exceptions.
- For Scala 3 and ZIO code (writing or reviewing it), follow `zen-of-scala`.
- For design, domain modeling, effects and testing strategy, follow `zen-of-james`.
- Validation: `./sbta "Test / compile; testFull; stage"`.
- MCP server `sbta` (this build's `./sbta --mcp`, configured in `.mcp.json` and `.kiro/settings/mcp.json`, approved in `.claude/settings.json`): use it for all sbt work: `compile`, `test` (`full` for all), `run`, `check` after editing Scala 3 sources, `glob-search` / `inspect` / `symbol-location` for symbols, `sbt-task` for any other command. Its tools are deferred in Claude Code: load them with ToolSearch (search `sbta`). If it's unavailable, say so and use `./sbta <command>`. Logs: `project/target/sbta/mcp.log`.
- Maintenance routine: `.factory/MAINTENANCE.md` (weekly), following the `zen-of-projects` Skill.

## Exceptions to `zen-of-projects`

- `-language:strictEquality` applies to main sources only (`Test / scalacOptions` removes it). The tests compare `Either`, `Chunk`, `Duration`, and similar library types that have no `CanEqual` evidence; migrating them is deferred.
