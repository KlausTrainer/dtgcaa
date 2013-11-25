%% @doc dtgcaa_gif.
%%      Please see https://github.com/mochi/erl_img for an
%%      actual (full-fledged) gif-parser amongst others.
-module(dtgcaa_gif).

%% API
-export([contains_animation/3]).

-define(EXTENSION, 16#21). %% $!
-define(IMAGE,     16#2c). %% $,
-define(TRAILER,   16#3b). %% $;

-define(MAGIC87, $G,$I,$F,$8,$7,$a).
-define(MAGIC89, $G,$I,$F,$8,$9,$a).

%% External API

-spec contains_animation(binary(), non_neg_integer(), 0..2) ->
          {stream_next, non_neg_integer(), 0..2} |
          {error, no_gif} |
          {ok, true | false}.
contains_animation(_Data, _Offset, _PixmapCount = 2) ->
    {ok, true};
contains_animation(Data, Offset = 0, PixmapCount) when byte_size(Data) < 6->
    {stream_next, Offset, PixmapCount};
contains_animation(Data, _Offset = 0, PixmapCount) ->
    case magic(Data) of
    false -> {error, no_gif};
    true -> contains_animation(Data, 6, PixmapCount)
    end;
contains_animation(Data, Offset = 6, PixmapCount) when byte_size(Data) - Offset < 7->
    {stream_next, Offset, PixmapCount};
contains_animation(Data, Offset = 6, PixmapCount) ->
    <<_:Offset/binary,_Width:16/little,_Height:16/little,Map:1,_Cr:3,
      _Sort:1,Pix:3,_Background:8,_AspectRatio:8,_/binary>> = Data,
    PaletteSize = case Map of
    0 -> 0;
    1 -> (1 bsl (Pix + 1)) * 3
    end,
    contains_animation(Data, Offset + 7 + PaletteSize, PixmapCount);
contains_animation(Data, Offset, PixmapCount) when byte_size(Data) - Offset < 1 ->
    {stream_next, Offset, PixmapCount};
contains_animation(Data, Offset, PixmapCount) ->
    case Data of
    <<_:Offset/binary,?EXTENSION,_/binary>> ->
        case read_blocks(Data, Offset + 1 + 1) of
        stream_next ->
            {stream_next, Offset, PixmapCount};
        {ok, Offset2} ->
            contains_animation(Data, Offset2, PixmapCount)
        end;
    <<_:Offset/binary,?IMAGE,_/binary>> ->
        case byte_size(Data) - Offset < 9 of
        true ->
            {stream_next, Offset, PixmapCount};
        false ->
            Offset1 = Offset + 1,
            <<_:Offset1/binary,_Left:16/little,_Top:16/little,
              _Width:16/little,_Height:16/little,Map:1,_Interlaced:1,
              _Sort:1,_:2,Pix:3,_/binary>> = Data,
            Offset2 = Offset1 + 9,
            Offset3 = case Map of
            0 -> Offset2;
            1 -> Offset2 + (1 bsl (Pix + 1)) * 3
            end,
            case read_blocks(Data, Offset3 + 1) of
            stream_next ->
                {stream_next, Offset, PixmapCount};
            {ok, NewOffset} ->
                contains_animation(Data, NewOffset, PixmapCount + 1)
            end
        end;
    <<_:Offset/binary,?TRAILER,_/binary>> ->
        {ok, false};
    <<_:Offset/binary,_/binary>> ->
        contains_animation(Data, Offset + 1, PixmapCount)
    end.


%% Internal API

magic(<<?MAGIC87,_/binary>>) -> true;
magic(<<?MAGIC89,_/binary>>) -> true;
magic(_) -> false.

read_block(Data, Offset) when byte_size(Data) - Offset < 1 ->
    stream_next;
read_block(Data, Offset) ->
    case Data of
    <<_:Offset/binary,0,_/binary>> ->
        terminator;
    <<_:Offset/binary,Size,_/binary>> ->
        Offset1 = Offset + 1,
        case byte_size(Data) - Offset1 < Size of
        true -> stream_next;
        false -> {ok, Offset1 + Size}
        end
    end.

read_blocks(Data, Offset) ->
    case read_block(Data, Offset) of
    stream_next -> stream_next;
    terminator -> {ok, Offset + 1};
    {ok, NewOffset} -> read_blocks(Data, NewOffset)
    end.
