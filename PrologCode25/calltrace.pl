loc_in(atlanta, georgia).
loc_in(hosutan, texas).
loc_in(austin, texas).
loc_in(toronto, ontario).
loc_in(X,usa):-loc_in(X,georgia).
loc_in(X,usa):-loc_in(X,texas).
loc_in(X,canada):-loc_in(X,ontaria).
