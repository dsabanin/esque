-define(QUEUE_WORKER_PREFIX, "esque_worker_").
-define(QUEUE_SUB_POSTFIX, "_sub").
-define(NAMESPACE, "esque").
-define(IDLE_PERIOD_MS, 1000).
-define(QUEUE_KEY(QueueName), ?NAMESPACE ++ ":queue:" ++ QueueName).
-define(QUEUE_SUB_KEY(QueueName), ?QUEUE_KEY(QueueName) ++ ?QUEUE_SUB_POSTFIX).
-define(QUEUE_NAME(QueueProps),
  case proplists:get_value(node_specific, QueueProps, false) of
    true -> atom_to_list(node()) ++ "_" ++ proplists:get_value(name, QueueProps);
    false -> proplists:get_value(name, QueueProps)
  end).
-define(QUEUE_FORMAT(QueueProps),
  ((fun() ->
      case proplists:get_value(format, QueueProps, default) of
        default -> {ok, Format} = application:get_env(esque, format), Format;
        Format -> Format
      end))().
-define(QUEUE_KEY_FAILED(QueueKey),QueueKey ++ "_failed").
