data List a = Nil  | Cons a (List a);

nrev = \xs -> case xs of {
	Nil -> Nil; 
	Cons y ys -> app (nrev y) (Cons y Nil);
};

app = \xs ys -> case xs of {
	Nil -> ys; 
	Cons z zs -> Cons z (app zs ys); 
};