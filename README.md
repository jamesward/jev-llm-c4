# Jev vs LLM Connect Four



## Configure

Use a Bedrock long-term API key and a TypeSafe AI key:

```bash
export AWS_BEARER_TOKEN_BEDROCK=YOUR_BEDROCK_TOKEN
export TYPESAFE_API_KEY=YOUR_TYPESAFE_TOKEN
```

Startup fails before model checks or server initialization if either variable is missing or blank.

## Run

```bash
./sbt dev
```

## Deploy to Heroku

This app uses `heroku/jvm` followed by `https://github.com/jamesward/buildpack-scala`. The buildpack stages the native-packager app, turns that staging directory into the slug root, and automatically runs the generated `bin/jev-llm-connect-four` launcher—no `Procfile` is needed.

The server binds to all interfaces and uses the `PORT` environment variable when present. Configure both API keys and keep one web dyno because active games are in memory:

```bash
heroku config:set --app jev-llm-c4 \
  AWS_BEARER_TOKEN_BEDROCK=YOUR_BEDROCK_TOKEN \
  TYPESAFE_API_KEY=YOUR_TYPESAFE_TOKEN
heroku ps:scale --app jev-llm-c4 web=1
```
