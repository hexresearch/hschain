------------------------------- MODULE pbft2 -------------------------------

\* Простой PBFT: каждую высоту возможен полный раунд; византийства нет.
\*   Проверяется, что можно дойти до заданной высоты.
\*   NewHeight -> Propose -> Prevote -> Precommit -> Commit

EXTENDS TLC, Sequences, Integers

PT == INSTANCE PT

CONSTANTS Workers, MaxHeight

(*--algorithm pbft2

variables
  messages = [w \in DOMAIN(Workers) |-> <<>>];


macro send_to_all(msg) begin
  messages := [ i \in DOMAIN(Workers) |-> Append(messages[i], msg) ];
end macro;


(*
macro send_to_others(msg) begin
  messages := [   i \in DOMAIN(Workers)
              |-> IF i = self THEN messages[i] ELSE Append(messages[i], msg)
              ];
end macro;

*)


macro recv() begin
  await messages[self] /= <<>>;
  msg := Head(messages[self]);
  messages[self] := Tail(messages[self]);
end macro;

macro reset_to_start() begin
  current_state := "NewHeight";
  number_of_prevotes := 0;
  number_of_precommits := 0;
end macro;

macro next_round() begin
  reset_to_start();
  current_round := current_round + 1;
  goto Start;
end macro;


macro log(msg) begin
  print("H:" \o ToString(current_height) \o " " \o
        "R:" \o ToString(current_round)  \o " " \o
        current_state \o " " \o
        Workers[self] \o
        ": " \o msg);
end macro;


\* ####################################################################################################################


process node \in DOMAIN(Workers)
variables
  current_height = 0,
  current_round = 0,
  current_state = "NewHeight",
  msg = <<>>,
  iam_proposer = FALSE,
  number_of_prevotes = 0,
  number_of_precommits = 0
  ;

begin
(*
  Цикл по высоте:
    case по текущему состоянию:
      newheight:
        если я пропозер, раздать всем сообщение об этом
        иначе ждать сообщения об этом
      prevote:
        если дождались более чем 2/3+ голосов -- переходить к precommit
        /* то есть: принять сообщение, классифицировать его, и либо новый цикл, либо снова (GOTO)
      precommit:
        если дождались более чем 2/3+ голосов -- переходить к commit
        /* то есть: принять сообщение, классифицировать его, и либо новый цикл, либо снова (GOTO)
      commit:
        //..?
        если дождались более чем 2/3+ голосов -- переходить к newheight
        /* то есть: принять сообщение, классифицировать его, и либо новый цикл, либо снова (GOTO)
*)

  Start:
    while current_height < MaxHeight do
      iam_proposer := (current_height + current_round) % Len(Workers) = (self - 1);
      if current_state = "NewHeight" then StateNewHeight:
        if iam_proposer then
          \* log("I'm PROPOSER!");
          send_to_all(<<"Propose", current_height, current_round, Workers[self]>>);
        end if;
        StateNewHeightGoNext: current_state := "Propose";
      \* ##############################################################################################################
      elsif current_state = "Propose" then StatePropose:
        recv();
        if msg[1] = "Propose" /\ msg[2] = current_height /\ msg[3] = current_round then
          \* получили правильный Propose, подтверждаем это и переключаемся на новое состояние
          StateProposeGoNext:
            send_to_all(<<"PreVote", current_height, current_round, Workers[self]>>);
            current_state := "PreVote";
        else
          log("Unordered message: " \o ToString(msg));
          assert(FALSE);
          next_round();
        end if;
      \* ##############################################################################################################
      elsif current_state = "PreVote" then StatePreVote:
        recv();
        if msg[1] = "PreVote" /\ msg[2] = current_height /\ msg[3] = current_round then
          \* получили правильный PreVote, подтверждаем это и переключаемся на новое состояние
          number_of_prevotes := number_of_prevotes + 1;
          if number_of_prevotes = Len(Workers) then
            StatePreVoteGoNext:
              send_to_all(<<"PreCommit", current_height, current_round, Workers[self]>>);
              current_state := "PreCommit";
          end if;
        else
          log("Unordered message: " \o ToString(msg));
          assert(FALSE);
          next_round();
        end if;
      \* ##############################################################################################################
      elsif current_state = "PreCommit" then StatePreCommit:
        recv();
        if msg[1] = "PreCommit" /\ msg[2] = current_height /\ msg[3] = current_round then
          \* получили правильный PreCommit, подтверждаем это и переключаемся на новое состояние
          number_of_precommits := number_of_precommits + 1;
          if number_of_precommits = Len(Workers) then
            StatePreCommitGoNext:
              \* send_to_all(<<"Commit", current_height, current_round>>);
              current_state := "Commit";
          end if;
        else
          log("Unordered message: " \o ToString(msg));
          assert(FALSE);
          next_round();
        end if;
      \* ##############################################################################################################
      elsif current_state = "Commit" then StateCommit:
        reset_to_start();
        current_round := 0;
        current_height := current_height + 1;
        goto Start;
      else
        log("Wrong state: " \o current_state);
        assert(FALSE);
      end if;
    end while;
end process;

end algorithm;

*)
\* BEGIN TRANSLATION
\* END TRANSLATION



=============================================================================
