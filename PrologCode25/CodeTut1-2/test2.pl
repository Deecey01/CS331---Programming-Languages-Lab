drinks(raj, tea).
drinks(mann, coffee).
drinks(sagar, juice).
drinks(raj, coffee).
drinks(kush, coffee).
pair(X, Y, Z) :-drinks(X, Z), drinks(Y, Z), X \== Y.
