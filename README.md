MiniErlangBlockchain: an exercise in Erlang
===========================================

Version V0.1
==============

Chiamiamo NODE un nodo della blockchain.
Ogni NODE sarà un attore Erlang su un nodo Erlang.

## Algoritmo di mantenimento della topologia:
 - {ping, Mittente, Nonce}
   rispondete Mittente ! {pong, Nonce}
 - {get_friends, Mittente, Nonce}
   rispondete Mittente ! {friends, Nonce, Lista_di_amici} 

 Nota: ci sarà un nodo creato dal docente il cui PID dell'attore
       e il cui indirizzo del nodo saranno pubblici e statici
       al quale potente mandare richieste {friends,...}

@ una transazione è una coppia {IDtransazione, Payload}

## Algoritmo di gossiping per le transazioni
 - {push, Transazione}  dove IDtransazione è un Nonce
   * lo ritrasmettete ai vostri amici
   * tenete conto della transazione per cercare di inserirla nei
     prossimi blocchi che create

@ un blocco è una coppia {IDnuovo_blocco,IDblocco_precedente, Lista_di_transazioni}
  IDblocco_precedente = none  se è il primo blocco della catena

## Algoritmo di gossiping per i blocchi
 - {update, Blocco}
   * lo ritrasmettete ai vostri amici
   * fate update della vostra visione della catena, eventualmente usando
     l'algoritmo di ricostruzione della catena e decidendo quale è la catena
     più lunga

## Algoritmo di ricostruzione della catena
  da chiamarsi quando vi arriva un update e non conoscete il blocco precedente, per esempio se vi siete
  appena connessi alla blockchain:
 - {get_previous, Mittente, Nonce, Idblocco_precedente}
   rispodente con  Mittente ! {previous, Nonce, Blocco}
 - {get_head, Mittente, Nonce}
   rispondete con Mittente ! {head, Nonce, Blocco} che è la cima dello stack (più
   lungo)


## COMPORTAMENTO DI UN NODO

0. il nodo professore NON partecipa alla blockchain e NON lo dovete
   usare come amico
1. ogni nodo cerca di mantenere sempre una lista di amici
   di lunghezza 3. Quando uno o più amici diventano inacessibili
   chiedete ai rimasti una lista di amici e randomicamente ne
   scegliete alcuni per tornare a quota 3. In caso perdiate tutti
   e 3 gli amici chiedete al nodo professore
2. mantenete l'insieme delle transazioni ancora da includere nei blocchi;
   l'insieme aumenta quando ricevete una push, cala quando includete delle
   transazioni in un blocco che minate oppure quando accettate un blocco
   ricevuto con una update (accettare = il blocco entra nella catena che per voi
   è la più lunga)
3. per minare un blocco dovete risolvere un problema computazionalmente costoso
   ovvero attendete N secondi dove N è un numero casuale fra 20s e 30s. Un blocco
   può contenere al massimo 10 transazioni
4. ogni volta che inviate un messaggio, ci deve essere una probabilità su 10
   di perderlo (= non inviarlo) e una su 10 di inviarne due copie
