MiniErlangBlockchain: an exercise in Erlang
===========================================

Version V0.5
============

Chiamiamo NODE un nodo della blockchain.
Ogni NODE sarà un attore Erlang su un nodo Erlang.

## Algoritmo di mantenimento della topologia:
 - {ping, Mittente, Nonce}
   rispondete Mittente ! {pong, Nonce}
 - {get_friends, Mittente, Nonce}
   rispondete Mittente ! {friends, Nonce, Lista_di_amici} 

 Nota: ci sarà un nodo creato dal docente il cui indirizzo del nodo
       Erlang sarà pubblico e statico e il cui PID verrà registrato
       globalmente come teacher_node. Tale nodo implementa
       esclusivamente l'algoritmo di mantenimento della topologia

@ una transazione è una coppia {IDtransazione, Payload}

## Algoritmo di gossiping per le transazioni
 - {push, Transazione}  dove IDtransazione è un Nonce
   * se non lo conoscevate già:
     * lo ritrasmettete ai vostri amici
     * tenete conto della transazione per cercare di inserirla nei
       prossimi blocchi che create

@ un blocco è una tripla {IDnuovo_blocco,IDblocco_precedente, Lista_di_transazioni, Soluzione}
  IDblocco_precedente = none  se è il primo blocco della catena
  Soluzione è ottenuta invocando proof_of_work:solve({IDblocco_precedente,Lista_di_transazioni}) e può essere
  verificata tramite proof_of_work:check({IDblocco_precedente,Lista_di_transazioni}, Soluzione)

## Algoritmo di gossiping per i blocchi
 - {update, Sender, Blocco}
   * se non lo conoscevate già:
     * verificate che il blocco sia corretto usando proof_of_work:check(...)
     * lo ritrasmettete ai vostri amici
     * fate update della vostra visione della catena, eventualmente usando
       l'algoritmo di ricostruzione della catena (chiedendo al Sender o agli amici) e
       decidendo quale è la catena più lunga

## Algoritmo di ricostruzione della catena
  da chiamarsi quando vi arriva un update e non conoscete il blocco precedente, per esempio se vi siete
  appena connessi alla blockchain:
 - {get_previous, Mittente, Nonce, Idblocco_precedente}
   rispondete con  Mittente ! {previous, Nonce, Blocco} non appena lo conoscete
 - {get_head, Mittente, Nonce}
   rispondete con Mittente ! {head, Nonce, Blocco} che è la cima dello stack (più
   lungo)


## COMPORTAMENTO DI UN NODO

0. il nodo professore NON partecipa alla blockchain e NON lo dovete
   usare come amico
1. ogni nodo cerca di mantenere sempre una lista di amici
   di lunghezza 3. Quando uno o più amici diventano inacessibili
   chiedete ai rimasti una lista di amici e randomicamente ne
   scegliete alcuni (diversi da voi!) per tornare a quota 3. In caso
   non riusciate a ripristinare i 3 amici chiedendo ai vostri amici
   potete ricorrere al nodo professore
2. mantenete l'insieme delle transazioni ancora da includere nei blocchi;
   l'insieme aumenta quando ricevete una push, cala quando includete delle
   transazioni in un blocco che minate oppure quando accettate un blocco
   ricevuto con una update (accettare = il blocco entra nella catena che per voi
   è la più lunga)
3. per minare un blocco dovete risolvere un problema computazionalmente costoso
   invocando proof_of_work:solve({IDblocco_precedente,Lista_di_transazioni}) sulla lista di transazioni
   che volete includere nel blocco.
   Un blocco può contenere al massimo 10 transazioni
4. ogni volta che inviate un messaggio, ci deve essere una probabilità su 10
   di perderlo (= non inviarlo) e una su 10 di inviarne due copie
