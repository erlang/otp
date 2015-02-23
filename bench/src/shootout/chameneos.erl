%%% The Great Computer Language Shootout
%%% http://shootout.alioth.debian.org/
%%% 
%%% contributed by Isaac Gouy (Erlang novice)

-module(chameneos). 
-export([main/1,complement/2,creature/4,meetingPlace/3]). 

-export([small/0,medium/0,big/0]).

small() -> 1000000.
medium() -> 5000000.
big() -> 10000000.


meetingPlace(N, Creature, Colour) -> 
    receive                                              
        {SomeCreature, SomeColour} ->   
            case Creature of
               first ->   
                  case N of
                     0 -> 
                        SomeCreature ! faded,
                        meetingPlace(N, first, first);
                     _ -> 
                        meetingPlace(N-1, SomeCreature, SomeColour) 
                  end;
                                                           
               _ ->               
                  Creature ! SomeColour,
                  SomeCreature ! Colour,
                  meetingPlace(N, first, first)            
            end;                       
            
        done -> 
            ok            
    end.


creature(Count, Colour, MeetingPlace, Main) ->      
   MeetingPlace ! {self(),Colour},
   receive  
      faded -> 
         Main ! {self(),Count};                                                           
      OtherColour -> 
         creature(Count+1, complement(Colour,OtherColour), MeetingPlace, Main)
   end.
      
   
complement(Colour,OtherColour) ->
   if 
      Colour == OtherColour -> Colour;
            
      true ->       
         case Colour of 
            blue -> 
               case OtherColour of red -> yellow; yellow -> red end;                     
            red ->
               case OtherColour of blue -> yellow; yellow -> blue end;                                       
            yellow ->      
               case OtherColour of blue -> red; red -> blue end 
         end                         
   end.


meetingCount([],Meetings) -> Meetings;
meetingCount(Creatures,Meetings) -> 
   receive 
      {Pid,Count} ->    
         meetingCount(lists:delete(Pid,Creatures), Meetings+Count)
   end.   


main(Arg) ->
    N = Arg, 
    MeetingPlace = spawn(chameneos, meetingPlace, [N,first,first]),
    Creatures = lists:map(
      fun(C) -> spawn(chameneos, creature, [0,C,MeetingPlace,self()]) end,
      [blue,red,yellow,blue]
      ),             
    Meetings = meetingCount(Creatures,0),                        
    MeetingPlace ! done,                       
    io:format("~w~n", [Meetings]),
    exit(ok).
