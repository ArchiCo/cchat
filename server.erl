-module(server).
-export([start/1, stop/1]).

% server state
-record(serverSt, {
  channels = [],
  nicks = []
}).

%channel state
-record(channelSt, {
  name,
  users = []
}).

start(ServerAtom) ->
  genserver:start(ServerAtom, #serverSt{}, fun handle_server/2).
stop(ServerAtom) ->
  genserver:stop(ServerAtom).

handle_server(State, Data) ->
  case Data of
    % join channel, check existence of channel and nickname, join the (new or original)state
    {join, Channel, Nick, Sender} ->
      % check the existence of channel
      case lists:member(Channel, State#serverSt.channels)of
      true ->
        Response = (catch (genserver:request(list_to_atom(Channel), {join, Sender}))),
        case Response of
          error ->
            {reply, error, State};
          join ->
            case lists:member(Nick, State#serverSt.nicks)of
              true ->
                {reply, join, State};
              false ->
                {reply, join, State#serverSt{nicks = [Nick | State#serverSt.nicks]}}
            end
        end;
      % start a new channel and join the user if it is not exist
      false ->
          genserver:start(list_to_atom(Channel), #channelSt{name = Channel, users = [Sender]}, fun handle_channel/2),
          {reply, join, State#serverSt{channels = [Channel | State#serverSt.channels]}}
      end;
    {leave, Channel, Sender} ->
      % check the user is in the channel
      case lists:member(Channel, State#serverSt.channels)of
      true -> Response = (catch (genserver:request(list_to_atom(Channel), {leave, Sender}))),
        case Response of
          leave -> {reply, leave, State};
          % user not in the channel
          error -> {reply, error, State}
        end;
        % channel not exist
      false -> {reply, error, State}
      end;
    % check if nick exists
    {nick, Nick} ->
     case lists:member(Nick, State#serverSt.nicks) of
       true -> {reply, error, State};
       false -> {reply, ok, State#serverSt{nicks = [Nick | State#serverSt.nicks]}}
      end;
    stop ->
      [genserver:stop(list_to_atom(Channel)) || Channel <- State#serverSt.channels]
  end.

handle_channel(State, Data) ->
  case Data of
    {join, Sender} ->
      case lists:member(Sender, State#channelSt.users) of
        % if sender is in the channel return error
        true -> {reply, error, State};
        false -> {reply, join, State#channelSt{users = [Sender | State#channelSt.users]}}
      end;
    {leave, Sender} ->
      case lists:member(Sender, State#channelSt.users) of
        true -> NewUser = lists:delete(Sender, State#channelSt.users),
        {reply, leave, State#channelSt{users = NewUser}};
        % if user is not the member of channel return error
        false -> {reply, error, State}
      end;
    {message_send, Nick, Msg, Sender} ->
      % check if the sender is the member of channel
      case lists:member(Sender, State#channelSt.users) of
        true ->
          spawn(
            % send message to every user in the channel
            fun() ->
              [genserver:request(
                Receiver, {message_receive, State#channelSt.name, Nick, Msg}
              )
                || Receiver <- State#channelSt.users, Receiver =/= Sender]
            end
          ), {reply, message_send, State};
        % if the sender not member then error
        false ->
          {reply, error, State}
      end
  end.


