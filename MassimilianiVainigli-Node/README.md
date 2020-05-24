# Implementazione di una blockchain in Erlang
Autori: Lorenzo Massimiliani, Lorenzo Vainigli.

#### Struttura del codice

- **block_chain.erl** implementa le operazioni che vengono effettuate sulla struttura dati che rappresenta la blockchain.
    - *mining()* si occupa di creare nuovi blocchi con l'operazione di mining.
    - *block_chain()* è responsabile dell'aggiunta di nuovi blocchi nella blockchain. Implementa l'***algoritmo di gossiping per i blocchi***.
    - *ricostruzioneCatena()* risolve i casi di fork tra la blockchain del nodo corrente e quella di un'altro, ovvero implemeta l'***algoritmo di ricostruzione della catena***.
- **friends.erl**
    - *check_nodes()* implementa l'***algoritmo di mantenimento della topologia*** della rete.
- **main.erl**
    - *compile()* è una piccola utility per per compilare tutti i moduli necessari.
    - ***start()*** è la funzione principale che serve a lanciare il programma.
- **manager.erl**
    - *manager()* è il nucleo del programma, ovvero l'attore che si occupa di gestire la comunicazione tra i diversi attori del nodo corrente e verso gli altri nodi (in entrata e in uscita). Inoltre implementa l'***algoritmo di gossiping per le transazioni***.
- **support.erl** contiene una serie di funzioni di utility.
- **test.erl** contine alcune funzioni per effettuare il testing del codice.