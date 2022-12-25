from prolog_structures import Rule, RuleBody, Term, Function, Variable, Atom, Number
from typing import List
from functools import reduce

import sys
import random

class Not_unifiable(Exception):
	pass

'''
Please read prolog_structures.py for data structures
that represent Prolog terms, rules, and goals.
'''
class Interpreter:
	def __init__(self):
		pass

	'''
	Example
	occurs_check (v, t) where v is of type Variable, t is of type Term.
	occurs_check (v, t) returns true if the Prolog Variable v occurs in t.
	Please see the lecture note Control in Prolog to revisit the concept of
	occurs-check.
	'''
	def occurs_check (self, v : Variable, t : Term) -> bool:
		if isinstance(t, Variable):
			return v == t
		elif isinstance(t, Function):
			for t in t.terms:
				if self.occurs_check(v, t):
					return True
			return False
		return False


	'''
	Problem 1
	variables_of_term (t) where t is of type Term.
	variables_of_clause (c) where c is of type Rule.

	The function should return the Variables contained in a term or a rule
	using Python set.

	The result must be saved in a Python set. The type of each element (a Prolog Variable)
	in the set is Variable.
	'''
	def variables_of_term (self, t : Term) -> set :
		variables = set()
		if isinstance(t, Variable):
			variables.add(t)
		if isinstance(t, Function):
			for term in t.terms:
				variables = variables.union(self.variables_of_term(term))
		return variables

	def variables_of_clause (self, c : Rule) -> set :
		variables = set()
		for term in c.head.terms:
			variables = variables.union(self.variables_of_term(term))
		for term in c.body.terms:
			variables = variables.union(self.variables_of_term(term))
		return variables


	'''
	Problem 2
	substitute_in_term (s, t) where s is of type dictionary and t is of type Term
	substitute_in_clause (s, t) where s is of type dictionary and c is of type Rule,

	The value of type dict should be a Python dictionary whose keys are of type Variable
	and values are of type Term. It is a map from variables to terms.

	The function should return t_ obtained by applying substitution s to t.

	Please use Python dictionary to represent a subsititution map.
	'''
	def substitute_in_term (self, s : dict, t : Term) -> Term:
		if isinstance(t, Function):
			new_terms = []
			for term in t.terms:
				new_terms.append(self.substitute_in_term(s, term))
			return Function(t.relation, new_terms)
		if isinstance(t, Variable):
			if t in s:
				return s[t]
		return t


	def substitute_in_clause (self, s : dict, c : Rule) -> Rule:
		if len(c.body.terms) == 0:
			return Rule(self.substitute_in_term(s, c.head), c.body)
		return Rule(self.substitute_in_term(s, c.head), RuleBody([self.substitute_in_term(s, term) for term in c.body.terms]))


	'''
	Problem 3
	unify (t1, t2) where t1 is of type term and t2 is of type Term.
	The function should return a substitution map of type dict,
	which is a unifier of the given terms. You may find the pseudocode
	of unify in the lecture note Control in Prolog useful.

	The function should raise the exception raise Not_unfifiable (),
	if the given terms are not unifiable.

	Please use Python dictionary to represent a subsititution map.
	'''
	def unify (self, t1: Term, t2: Term) -> dict:
		return self.unify_helper(t1, t2, {})


	def unify_helper(self, X: Term, Y: Term, theta: dict) -> dict:
		X = self.substitute_in_term(theta, X)
		Y = self.substitute_in_term(theta, Y)

		if isinstance(X, Variable) and not self.occurs_check(X, Y):
			for term in theta:
				theta[term] = self.substitute_in_term({X: Y}, theta[term])
			theta[X] = Y
			return theta

		if isinstance(Y, Variable) and not self.occurs_check(Y, X):
			for term in theta:
				theta[term] = self.substitute_in_term({Y: X}, theta[term])
			theta[Y] = X
			return theta

		if X == Y:
			return theta

		if isinstance(X, Function) and isinstance(Y, Function) and X.relation == Y.relation and len(X.terms) == len(Y.terms):
			for i in range(len(X.terms)):
				theta = self.unify_helper(X.terms[i], Y.terms[i], theta)
			return theta

		raise Not_unifiable()


	fresh_counter = 0
	def fresh(self) -> Variable:
		self.fresh_counter += 1
		return Variable("_G" + str(self.fresh_counter))
	def freshen(self, c: Rule) -> Rule:
		c_vars = self.variables_of_clause(c)
		theta = {}
		for c_var in c_vars:
			theta[c_var] = self.fresh()

		return self.substitute_in_clause(theta, c)


	'''
	Problem 4
	Following the Abstract interpreter pseudocode in the lecture note Control in Prolog to implement
	a nondeterministic Prolog interpreter.

	nondet_query (program, goal) where
		the first argument is a program which is a list of Rules.
		the second argument is a goal which is a list of Terms.

	The function returns a list of Terms (results), which is an instance of the original goal and is
	a logical consequence of the program. See the tests cases (in src/main.py) as examples.
	'''
	def nondet_query (self, program : List[Rule], pgoal : List[Term]) -> List[Term]:
		while True:
			resolvent = pgoal.copy()
			while len(resolvent) > 0:
				A = random.choice(resolvent)

				unifiable_clauses = []
				for clause in program:
					try:
						theta = self.unify(A, clause.head)
						unifiable_clauses.append((theta, clause))
					except:
						pass
				if len(unifiable_clauses) == 0:
					break

				theta, A_prime = random.choice(unifiable_clauses)

				resolvent.remove(A)
				resolvent.extend(A_prime.body.terms)

				for i in range(len(resolvent)):
					resolvent[i] = self.substitute_in_term(theta, resolvent[i])
				for i in range(len(pgoal)):
					pgoal[i] = self.substitute_in_term(theta, pgoal[i])

			if len(resolvent) == 0:
				return pgoal


	'''
	Challenge Problem

	det_query (program, goal) where
		the first argument is a program which is a list of Rules.
		the second argument is a goal which is a list of Terms.

	The function returns a list of term lists (results). Each of these results is
	an instance of the original goal and is a logical consequence of the program.
	If the given goal is not a logical consequence of the program, then the result
	is an empty list. See the test cases (in src/main.py) as examples.
	'''
	def det_query (self, program : List[Rule], pgoal : List[Term]) -> List[List[Term]]:
		solutions = []
		self.dfs(pgoal.copy(), pgoal.copy(), program, solutions)
		return solutions

	def dfs(self, resolvent: List[Term], goal: List[Term], program: List[Rule], solutions: List[List[Term]]):
		if len(resolvent) == 0:
				solutions.append(goal)
				return
		else:
			chosen_goal = resolvent.pop(0)
			for rule in program: 
				try:
					self.unify(chosen_goal, rule.head)
				except: 
					continue
				rule = self.freshen(rule) 
				theta = self.unify(chosen_goal, rule.head) 
				new_resolvent, new_goal = resolvent.copy(), goal.copy() 
				new_resolvent.extend(rule.body.terms)
				for i in range(0, len(new_resolvent)):
					new_resolvent[i] = self.substitute_in_term(theta, new_resolvent[i]) 
				for i in range(0, len(new_goal)):
					new_goal[i] = self.substitute_in_term(theta, new_goal[i]) 
				self.dfs(new_resolvent, new_goal, program, solutions)
