

start :-
		nl,
		stampa_formattato('****************************************************************************************',blue),nl,
		sleep(0.3),
		stampa_formattato("####################||| FAST DIAGNOSIS |||#########################",blue),nl,
		sleep(0.4),
		stampa_formattato('****************************************************************************************',blue),nl,nl,nl,
		sleep(0.4),
		presentation.

% -----------------------------------------------------------------------


    sintomo(Paziente,febbre) :- verifica(Paziente,"Hai la febbre (s/n) ? ", febbre).

    sintomo(Paziente,eruzione_cutanea) :- verifica(Paziente,"Hai eruzioni cutanee (s/n) ?", eruzione_cutanea).

    sintomo(Paziente,mal_di_testa) :- verifica(Paziente,"Hai mal di testa (s/n) ?", mal_di_testa).

    sintomo(Paziente,raffreddore) :- verifica(Paziente,"Hai raffreddore (s/n) ?", raffreddore).

    sintomo(Paziente,congiuntivite) :- verifica(Paziente,"Hai la congiuntivite (s/n) ?", congiuntivite).

    sintomo(Paziente,tosse) :- verifica(Paziente,"Hai la tosse (s/n) ?", tosse).

    sintomo(Paziente,male_al_corpo) :- verifica(Paziente,"Hai dolori al corpo (s/n) ?", male_al_corpo).

    sintomo(Paziente,brividi) :- verifica(Paziente," Hai i brividi (s/n) ?", brividi).

    sintomo(Paziente,gola_infiammata) :- verifica(Paziente,"Hai la gola infiammata (s/n) ?", gola_infiammata).

    sintomo(Paziente,starnuti) :- verifica(Paziente,"Ti viene da starnutire (s/n) ?", starnuti).

    sintomo(Paziente,ghiandole_gonfie) :- verifica(Paziente,"Hai a la ghiandole salivari ingrossate (s/n) ?", ghiandole_gonfie).

    sintomo(Paziente,vomito) :- verifica(Paziente, "Hai vomitato (s/n) ?", vomito).

    sintomo(Paziente,nausea) :- verifica(Paziente, "Hai la nausea (s/n) ?", nausea).

    sintomo(Paziente,confusione) :- verifica(Paziente, "Ti senti confuso/a (s/n) ?", confusione).

    sintomo(Paziente,dolori_nucali) :- verifica(Paziente, "Hai male alla nuca (s/n) ?", dolori_nucali).

    sintomo(Paziente,polidipsia) :- verifica(Paziente, "Hai una strana sensazione di sete (s/n) ?", polidipsia).

    sintomo(Paziente,astenia) :- verifica(Paziente, "Ti senti stanco/a (s/n) ?", astenia).

    sintomo(Paziente,poliuria) :- verifica(Paziente, "Hai notato un'aumentata quantita' di urine (s/n) ?", poliuria).

    sintomo(Paziente,perdita_di_peso) :- verifica(Paziente, "Hai notato uno strano calo di peso (s/n) ?", perdita_di_peso).

    sintomo(Paziente,dermatite) :- verifica(Paziente, "Hai riscontrato la dermatite (s/n) ?", dermatite).

    sintomo(Paziente,gonfiore_addominale) :- verifica(Paziente, "Hai del gonfiore addominale (s/n) ?", gonfiore_addominale).

    sintomo(Paziente,convulsioni) :- verifica(Paziente, "Hai avuto delle convulsioni (s/n) ?", convulsioni).

    sintomo(Paziente,difficolta_motorie) :- verifica(Paziente, "Hai avuto difficolta' motorie (s/n) ?", difficolta_motorie).

    sintomo(Paziente,visione_disturbata) :- verifica(Paziente, "Hai avuto la problemi di offuscamento della vista (s/n) ?",visione_disturbata).

	sintomo(Paziente,respiro_sibilante) :- verifica(Paziente, "Hai il respiro sibilante? (s/n)?", respiro_sibilante).

	sintomo(Paziente,costrizione_toracica) :- verifica(Paziente, "Senti un peso sul petto (s/n)?", costrizione_toracica).

	sintomo(Paziente,dispnea) :- verifica(Paziente, "Hai difficolta' a respirare (s/n)?", dispnea).

	sintomo(Paziente,tosse_secca) :- verifica(Paziente, "Hai la tosse secca (s/n)?", tosse_secca).

	sintomo(Paziente,diarrea) :- verifica(Paziente, "Hai avuto scariche di diarrea nelle ultime 3 ore (s/n)?", diarrea).

	sintomo(Paziente,dolore_addominale) :- verifica(Paziente, "Avverti dolore addominale o crampi (s/n)?", dolore_addominale).



	medico(rossi).
	medico(pellegrino).
	medico(luisi).
	medico(oliveto).
	medico(divino).
	medico(pareschi).
	medico(ferraro).



	specializzato_in(rossi,dermatologia).
	specializzato_in(rossi,malattie_infettive).
	specializzato_in(pellegrino,pneumologia).
	specializzato_in(pellegrino,malattie_apparato_respiratorio).
	specializzato_in(luisi,neurochirurgia).
	specializzato_in(luisi,oncologia).
	specializzato_in(luisi,radio_diagnostica).
	specializzato_in(oliveto,malattie_apparato_digerente).
	specializzato_in(divino,allergologia).
	specializzato_in(pareschi,malattie_infettive). %chi cura meningite
	specializzato_in(ferraro,endocrinologia). %diabete


	cura(paracetamolo,morbillo).
	cura(paracetamolo,rosolia).
	cura(paracetamolo,meningite).
	cura(paracetamolo,influenza).
	cura(paracetamolo,varicella).
	cura(paracetamolo,parotite).
	cura(ibuprofene,morbillo).
	cura(ibuprofene,rosolia).
	cura(ibuprofene,meningite).
	cura(ibuprofene,influenza).
	cura(cortisone,parotite).
	cura(cortisone,asma).
	cura(cortisone,allergia).
	cura(insulina,diabete).
	cura(steroidi,celiachia).
	cura(infliximab,celiachia).
	cura(chemioterapia,cancro_cervello).
	cura(radio_terapia,cancro_cervello).
	cura(broncodilatatore,asma).
	cura(broncodilatatore,allergia).
	cura(ossigeno,asma).
	cura(antistaminico,allergia).
	cura(antidiarroico,gastroenterite).
	cura(probiotico,gastroenterite).
	cura(codeina,gastroenterite).



	trattato_in(morbillo,malattie_infettive).
	trattato_in(morbillo,dermatologia).
	trattato_in(rosolia,malattie_infettive).
	trattato_in(rosolia,dermatologia).
	trattato_in(varicella,malattie_infettive).
	trattato_in(varicella,dermatologia).
	trattato_in(parotite,malattie_infettive).
	trattato_in(parotite,dermatologia).
	trattato_in(meningite,malattie_infettive).
	trattato_in(influenza,malattie_infettive).
	trattato_in(diabete,endocrinologia).
	trattato_in(celiachia,malattie_apparato_digerente).
	trattato_in(cancro_cervello,oncologia).
	trattato_in(cancro_cervello,neurochirurgia).
	trattato_in(cancro_cervello,radio_diagnostica).
	trattato_in(asma,allergologia).
	trattato_in(asma,malattie_apparato_respiratorio).
	trattato_in(asma,pneumologia).
	trattato_in(gastroenterite,malattie_infettive).
	trattato_in(gastroenterite,malattie_apparato_digerente).
	trattato_in(allergia,allergologia).


    ipotesi(Paziente,morbillo) :-
        sintomo(Paziente,raffreddore),
        sintomo(Paziente,febbre),
        sintomo(Paziente,tosse),
        sintomo(Paziente,congiuntivite),
		sintomo(Paziente,starnuti),
        sintomo(Paziente,eruzione_cutanea),!.

    ipotesi(Paziente,rosolia) :-
        sintomo(Paziente,febbre),
        sintomo(Paziente,mal_di_testa),
        sintomo(Paziente,raffreddore),
        sintomo(Paziente,eruzione_cutanea),!.

	ipotesi(Paziente,allergia) :-
		sintomo(Paziente,oculo_rinite),
		sintomo(Paziente,respiro_sibilante),
		sintomo(Paziente,tosse),
		sintomo(Paziente,starnuti),
		sintomo(Paziente,costrizione_toracica),!.

    ipotesi(Paziente,meningite) :-
		sintomo(Paziente,febbre),
		sintomo(Paziente,nausea),
		sintomo(Paziente,vomito),
		sintomo(Paziente,confusione),
		sintomo(Paziente,dolori_nucali),!.


    ipotesi(Paziente,influenza) :-
        sintomo(Paziente,mal_di_testa),
        sintomo(Paziente,starnuti),
        sintomo(Paziente,gola_infiammata),
        sintomo(Paziente,raffreddore),
        sintomo(Paziente,brividi),
		sintomo(Paziente,tosse),
		sintomo(Paziente,febbre),!.

    ipotesi(Paziente,parotite) :-
        sintomo(Paziente,febbre),
        sintomo(Paziente,ghiandole_gonfie),!.

    ipotesi(Paziente,varicella) :-
        sintomo(Paziente,febbre),
        sintomo(Paziente,brividi),
        sintomo(Paziente,male_al_corpo),
        sintomo(Paziente,eruzione_cutanea),!.

    ipotesi(Paziente,diabete) :-
		sintomo(Paziente,polidipsia), %sensazione di sete
		sintomo(Paziente,astenia),    %sensazione di stanchezza
		sintomo(Paziente,poliuria),
		sintomo(Paziente,perdita_di_peso),!.

    ipotesi(Paziente,celiachia) :-
		sintomo(Paziente,gonfiore_addominale),
		sintomo(Paziente,perdita_di_peso),
		sintomo(Paziente,astenia),
		sintomo(Paziente,dermatite),!.

    ipotesi(Paziente,cancro_cervello) :-
		sintomo(Paziente,convulsioni),
		sintomo(Paziente,nausea),
		sintomo(Paziente,vomito),
		sintomo(Paziente,mal_di_testa),
		sintomo(Paziente,difficolta_motorie),
		sintomo(Paziente,visione_disturbata),!.

	ipotesi(Paziente,asma) :-
		sintomo(Paziente,respiro_sibilante),
		sintomo(Paziente,costrizione_toracica),
		sintomo(Paziente,dispnea),
		sintomo(Paziente,tosse_secca),!.

	ipotesi(Paziente,gastroenterite) :-
		sintomo(Paziente,diarrea),
		sintomo(Paziente,dolore_addominale),
		sintomo(Paziente,vomito),
		sintomo(Paziente,mal_di_testa),
		sintomo(Paziente,brividi),!.


	ipotesi(_,"Nessuna malattia").



	ask(Paziente,Question,Sintomo) :-
		%write('[+] '),write(Paziente),write(', '),write(Question),
		stampa_formattato('[+] ',blue),stampa_formattato(Paziente,blue),stampa_formattato(', ',blue),stampa_formattato(Question,blue),
		read(N),

		write('Loading.'),nl,
		sleep(0.4),
		write('Loading..'),nl,
		sleep(0.5),
		write('Loading...'),nl,
		sleep(0.6),
		nl,



		((N == si ; N == s)
		             -> assert(yes(Question)),assert(fact(Sintomo));
		                assert(no(Question)),fail).

	% ------------------------------------------------------------------------


	:- dynamic yes/1, no/1, fact/1.

	verifica(P,S,X) :-
	   (yes(S) -> true ;
			(no(S) -> fail ; ask(P,S,X))).



	undo :- retract(yes(_)),fail.
	undo :- retract(no(_)),fail.
	undo :- retract(fact(_)),fail.
	undo.

	%predicato universale per le stampe formattate
	stampa_formattato(P,C):-
		ansi_format([bold,fg(C)],P,[]).



	stampa_formattato2(P,C):-
		write('\n'),write('     '),stampa_formattato('[-] ',C),stampa_formattato(P,C).


	stampa_sintomi(Malattia):-
	 	write('\nHo dedotto: '),stampa_formattato(Malattia,red),write(" perche' hai i seguenti sintomi: "),
	 	forall(fact(P),stampa_formattato2(P,green)).


	stampa_medicinale(Malattia) :-
		findall(X,cura(X,Malattia),L),
		scorri_lista(L).


	ottieni_medici_supporto([],_).
	ottieni_medici_supporto([H|T],P):-
		findall(Medico,specializzato_in(Medico,H),L),
		append(L,P,Ris),
		ottieni_medici_supporto(T,Ris).




	ottieni_medici([H|T]):-
		findall(Medico,specializzato_in(Medico,H),L),
		ottieni_medici_supporto(T,L),
		filtra(L).


	filtra(L):-
		rimuovi_duplicati(L,Ris),
		scorri_lista(Ris).



	stampa_medico_riferimento(Malattia) :-
		findall(X,trattato_in(Malattia,X),L), %specializzazioni che se conseguite da un medico consentono di curare una certa malattia
		ottieni_medici(L).


	stampa_conoscenza_malattie:-
		findall(Malattia,cura(_,Malattia),L),
		rimuovi_duplicati(L,L1),
		scorri_lista(L1).


	scorri_lista([]).
	scorri_lista([H|T]):-
		call(stampa_formattato2(H,red)),
		scorri_lista(T).


	rimuovi_duplicati([],[]).
	rimuovi_duplicati([H | T], L) :-
		member(H, T),
		rimuovi_duplicati(T, L).

	rimuovi_duplicati([H|T],[H|T1]) :- not(member(H,T)),
									   rimuovi_duplicati(T,T1).



	check_malattia(_,Malattia,[H|T]) :-
			(
	           (member(Malattia,[H|T]), not(Malattia=="Nessuna malattia"))-> fail
			   ;true
			).



	pt(Paziente):-
			ipotesi(Paziente,Malattia),%il predicato ipotesi "ritornera'" sempre true, a quel punto per mezzo del predicato definito vedo se la malattia e' stata diagnosticata oppure no


			findall(X,ipotesi(Paziente,X),L), %L viene avvolorata con findall
			(
				(not(check_malattia(Paziente,Malattia,L))) ->
									stampa_formattato('\n[+] Le malattie che sono in grado di diagnosticare sono: ',red),
									stampa_conoscenza_malattie,
									stampa_formattato('\n\n[+] ATTENZIONE ',red),
									stampa_formattato(Paziente,red),
			                        stampa_formattato(', potresti avere ',red),
			                        stampa_formattato(Malattia,red),
									stampa_formattato('.',red),
									write('\n\n'),
									stampa_formattato('[+] Terapia consigliata: ',red),
									stampa_medicinale(Malattia),
									write('\n\n'),
									stampa_formattato('[+] I medici specialisti ai quali puoi rivolgerti sono:',red),
									stampa_medico_riferimento(Malattia)
								   ;stampa_formattato('[+] Mi dispiace, non sono in grado di diagnosticare la malattia!',red)

			),

			nl,nl,
			not(Malattia=="Nessuna malattia") ->
					(
						write('Spiegazione analisi: (s/n)  '), %le malattia che sono in grado di diagnosticare sono:
						read(R),
						(
							(R==s ; R==si) -> stampa_sintomi(Malattia),undo,end
			                ;sleep(1)
						)
					);true,
			undo, %dato che non cuttiamo gli undo vengono eseguiti tra virgolette con conseguente retract dalla base di dati dei precedenti fatti asseriti
			end.


	% ------------------------------------------------------------------------

	end :-
			nl,nl,nl,nl,
			sleep(0.6),
			stampa_formattato('*****************************************************************************************',blue),nl,
			sleep(0.4),
			stampa_formattato("###################||| GRAZIE E ARRIVEDERCI |||######################",blue),nl,
			sleep(0.4),
			stampa_formattato('*****************************************************************************************',blue),nl.


	presentation :-
		write('[+] Ciao, benvenuto in EasyDiagnosis! Spero di poterti aiutare nella diagnosi.'),nl,
		write('[+] Come ti chiami ? '),
		read(N),nl,
		(N == @(null) -> nl,write('[+] Grazie per avermi usato!, Arrivederci'),nl,
						 end,fail
						 ;pt(N)
		).

	help :- write("Per avviare il sistema esperto digitare 'start.' nella shell interattiva."),nl.

	% ------------------------------------------------------------------------
