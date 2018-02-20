-module(server).
-export([start/1, stop/1]).

% Start a new server process with the given name

-record(serverState, {
  channels = [],
  nicks = []
}).

-record(channelState, {
  name,
  users = []
}).

start(ServerAtom) ->
  genserver:start(ServerAtom, #serverState{}, fun handle_server/2).
stop(ServerAtom) ->
  genserver:stop(ServerAtom).

handle_server(State, Data) ->
  case Data of
    % join channel, check existence of channel and nickname, join the (new or original)state
    % TODO: the checking of channel and nick be independent of this function
    {join, Channel, Nick, Sender} ->
      ChannelExists = lists:member(Channel, State#serverState.channels),
      if ChannelExists ->
        Response = (catch (genserver:request(list_to_atom(Channel), {join, Sender}))),
        case Response of
          error ->
            {reply, error, State};
          join ->
            NickExists = lists:member(Nick, State#serverState.nicks),
            if NickExists -> {reply, join, State};
              true ->
                {reply, join, State#serverState{nicks = [Nick | State#serverState.nicks]}}
            end
        end;
        % start a new channel and join the user if it is not exist
        true ->
          genserver:start(list_to_atom(Channel), #channelState{name = Channel, users = [Sender]}, fun handle_channel/2),
          {reply, join, State#serverState{channels = [Channel | State#serverState.channels]}}
      end;
    {leave, Channel, Sender} ->
      ChannelExists = lists:member(Channel, State#serverState.channels),
      if ChannelExists ->
        Response = (catch (genserver:request(list_to_atom(Channel), {leave, Sender}))),
        case Response of
          leave -> {reply, leave, State};
          % user not in the channel
          error -> {reply, error, State}
        end;
        true ->
          % channel not exist
          {reply, error, State}
      end;
    % check if nick exists
    {nick, Nick} ->
      NickExists = lists:member(Nick, State#serverState.nicks),
      if NickExists ->
        {reply, error, State};
        true ->
          % return nick to used by client checking
          {reply, ok, State#serverState{nicks = [Nick | State#serverState.nicks]}}
      end;
    stop ->
      [genserver:stop(list_to_atom(Channel)) || Channel <- State#serverState.channels]
  end.

handle_channel(State, Data) ->
  case Data of
    {join, Sender} ->
      IsMember = lists:member(Sender, State#channelState.users),
      if IsMember ->
        % if sender is in the channel return error
        {reply, error, State};
        true -> {reply, join, State#channelState{users = [Sender | State#channelState.users]}}
      end;
    {leave, Sender} ->
      IsMember = lists:member(Sender, State#channelState.users),
      if IsMember ->
        NewUser = lists:delete(Sender, State#channelState.users),
        {reply, leave, State#channelState{users = NewUser}};
        % if user is not in the channel return error
        true -> {reply, error, State}
      end;
    {message_send, Nick, Msg, Sender} ->
      IsMember = lists:member(Sender, State#channelState.users),
      case IsMember of
        true ->
          spawn(
            % send message to every user in the channel
            fun() ->
              [genserver:request(
                Receiver, {message_receive, State#channelState.name, Nick, Msg}
              )
                || Receiver <- State#channelState.users, Receiver =/= Sender]
            end
          ), {reply, message_send, State};
        % if the sender not member then error
        false ->
          {reply, error, State}
      end
  end.


