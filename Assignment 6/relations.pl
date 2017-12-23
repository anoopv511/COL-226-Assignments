male(duryodhana).
male(dushasana).
male(arjuna).
male(sahadeva).
male(nakula).
male(bhima).
male(yudhishthira).
male(shakuni).
male(karna).
male(janamejaya).
male(abhimanyu).
male(parikshit).
male(shrutakirti).
male(prativindhya).
male(shatanika).
male(sutasoma).
male(shrutakarma).
male(shantanu).
male(parashara).
male(bhishma).
male(chitrangada).
male(vichitravirya).
male(dhritarashtra).
male(vyasa).
male(pandu).

female(ganga).
female(satyavati).
female(ambika).
female(ambalika).
female(gandhari).
female(kunti).
female(madri).
female(duhsala).
female(subhadra).
female(uttara).
female(madravati).
female(draupadi).

married(ganga,shantanu).
married(satyavati,shantanu).
married(ambika,vichitravirya).
married(ambalika,vichitravirya).
married(gandhari,dhritarashtra).
married(kunti,pandu).
married(madri,pandu).
married(subhadra,arjuna).
married(uttara,abhimanyu).
married(madravati,parikshit).
married(draupadi,arjuna).
married(draupadi,bhima).
married(draupadi,yudhishthira).
married(draupadi,nakula).
married(draupadi,sahadeva).
married(duhsala,jayadratha).

child(bhishma,shantanu).
child(vichitravirya,shantanu).
child(chitrangada,shantanu).
child(vyasa,parashara).
child(dhritarashtra,vichitravirya).
child(pandu,vichitravirya).
child(duryodhana,dhritarashtra).
child(dushasana,dhritarashtra).
child(duhsala,dhritarashtra).
child(arjuna,pandu).
child(sahadeva,pandu).
child(nakula,pandu).
child(bhima,pandu).
child(yudhishthira,pandu).
child(abhimanyu,arjuna).
child(parikshit,abhimanyu).
child(prativindhya,yudhishthira).
child(shatanika,nakula).
child(sutasoma,bhima).
child(shrutakirti,sahadeva).
child(shrutakarma,arjuna).
child(janamejaya,parikshit).
child(shrutakirti,draupadi).
child(prativindhya,draupadi).
child(shatanika,draupadi).
child(sutasoma,draupadi).
child(shrutakarma,draupadi).
child(duryodhana,gandhari).
child(duhsala,gandhari).
child(dushasana,gandhari).
child(karna,kunti).
child(bhishma,ganga).
child(chitrangada,satyavati).
child(vichitravirya,satyavati).
child(dhritarashtra,ambika).
child(vyasa,satyavati).
child(pandu,ambalika).
child(arjuna,kunti).
child(bhima,kunti).
child(yudhishthira,kunti).
child(nakula,madri).
child(sahadeva,madri).
child(abhimanyu,subhadra).
child(parikshit,uttara).
child(janamejaya,madravati).

father(A,B):-
	child(A,B),
	male(B).

mother(A,B):-
	child(A,B),
	female(B).

brother(A,B):-
	father(A,C),
	father(B,C),
	mother(A,D),
	mother(B,D),
	male(B).

sister(A,B):-
	father(A,C),
	father(B,C),
	mother(A,D),
	mother(B,D),
	female(B).

sibling(A,B):-
	father(A,C),
	father(B,C),
	mother(A,D),
	mother(B,D).

step_brother(A,B):-
	father(A,C),
	father(B,C),
	mother(A,D),
	mother(B,E),
	D \= E,
	male(B).
step_brother(A,B):-
	mother(A,C),
	mother(B,C),
	father(A,D),
	father(B,E),
	D \= E,
	male(B).

step_sister(A,B):-
	father(A,C),
	father(B,C),
	mother(A,D),
	mother(B,E),
	D \= E,
	female(B).
step_sister(A,B):-
	mother(A,C),
	mother(B,C),
	father(A,D),
	father(B,E),
	D \= E,
	female(B).

step_mother(A,B):-
	mother(C,B),
	step_brother(A,C).

step_mother(A,B):-
	mother(C,B),
	step_sister(A,C).

step_father(A,B):-
	father(C,B),
	step_brother(A,C).

step_father(A,B):-
	father(C,B),
	step_sister(A,C).

husband(A,B):-
	married(A,B),
	male(B).

wife(A,B):-
	married(A,B),
	female(B).

grand_father(A,B):-
	father(A,C),
	father(C,B).
grand_father(A,B):-
	mother(A,C),
	father(C,B).

grand_mother(A,B):-
	father(A,C),
	mother(C,B).
grand_mother(A,B):-
	mother(A,C),
	mother(C,B).

aunt(A,B):-
	father(A,C),
	sister(C,B).
aunt(A,B):-
	mother(A,C),
	sister(C,B).
aunt(A,B):-
	father(A,C),
	brother(C,D),
	husband(B,D).
aunt(A,B):-
	mother(A,C),
	brother(C,D),
	husband(B,D).

uncle(A,B):-
	father(A,C),
	brother(C,B).
uncle(A,B):-
	mother(A,C),
	brother(C,B).
uncle(A,B):-
	father(A,C),
	sister(C,D),
	husband(D,B).
uncle(A,B):-
	mother(A,C),
	sister(C,D),
	husband(D,B).

niece(A,B):-
	aunt(B,A).

nephew(A,B):-
	uncle(B,A).

brother_in_law(A,B):-
	sister(A,C),
	husband(C,B).

sister_in_law(A,B):-
	brother(A,C),
	husband(B,C).

mother_in_law(A,B):-
	married(A,C),
	mother(C,B).
mother_in_law(A,B):-
	married(A,C),
	mother(C,B).

father_in_law(A,B):-
	married(A,C),
	father(C,B).
father_in_law(A,B):-
	married(A,C),
	father(C,B).

ancestor(A,B):-
	child(A,C),
	ancestor(C,B).

descendant(A,B):-
	child(A,C),
	descendant(C,B).

cousin(A,B):-
	niece(C,A),
	child(B,C).

cousin(A,B):-
	nephew(C,A),
	child(B,C).
