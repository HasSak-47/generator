export type Baz = {
	foo: FooBar,
	bar: Date,
	baz: Foo,
}

export type Foo = {
	foo: number,
	bar: string | null,
	baz: Date[],
}

export type FooBar = {tag: "Foo" | "Bar", data: Foo | Bar}

export type Bar = {
	foo: number,
	bar: string | null,
	baz: number[],
}


type _Baz = {
	foo: _FooBar,
	bar: string,
	baz: _Foo,
}

type _Foo = {
	foo: number,
	bar: string | null,
	baz: string[],
}

type _FooBar = {tag: "_Foo" | "Bar", data: _Foo | Bar}


function into_domain_Baz(m: _Baz){
let wire_m = {
	foo: into_domain_FooBar(m.foo),
	bar: new Date(m.bar),
	baz: into_domain_Foo(m.baz),
}
return wire_m

}

function into_wire_Baz(m: Baz){
let wire_m = {
	foo: into_wire_FooBar(m.foo),
	bar: m.bar.toISOString(),
	baz: into_wire_Foo(m.baz),
}
return wire_m

}


function into_domain_Foo(m: _Foo){
let wire_m = {
	foo: m.foo,
	bar: m.bar,
	baz: m.baz.map(e => { return new Date(e)}),
}
return wire_m

}

function into_wire_Foo(m: Foo){
let wire_m = {
	foo: m.foo,
	bar: m.bar,
	baz: m.baz.map(e => { return e.toISOString()}),
}
return wire_m

}


function into_domain_FooBar(m: _FooBar){
}

function into_wire_FooBar(m: FooBar){
}



export async function request_bar(str: string, _date: Date, bar: Bar, ){
	const date = _date.toISOString();
	let url = `/request_bar/`;
	const searchParams = new URLSearchParams();
	searchParams.set('str', str);

	searchParams.set('date', date);


	url = searchParams.size > 0 ? `${url}?${searchParams}` : url;
	let response = await fetch(url, {
		method: 'post',
		body: JSON.stringify({
			bar: bar
		}),
		headers: {
			'Content-Type': 'application/json'
		}
	});

	if(!response.ok)
		throw new Error(response.statusText);

	let j = await response.json();
	return j as Bar[]
}

export async function request_foo(str: string, _date: Date, _foo: Foo, ){
	const date = _date.toISOString();
	const foo = into_wire_Foo(_foo);
	let url = `/request_foo/`;
	const searchParams = new URLSearchParams();
	searchParams.set('str', str);

	searchParams.set('date', date);


	url = searchParams.size > 0 ? `${url}?${searchParams}` : url;
	let response = await fetch(url, {
		method: 'put',
		body: JSON.stringify({
			foo: foo
		}),
		headers: {
			'Content-Type': 'application/json'
		}
	});

	if(!response.ok)
		throw new Error(response.statusText);

	let j = await response.json();
	return j as Foo[]
}

export async function request_baz(str: string, _date: Date, _baz: Baz, ){
	const date = _date.toISOString();
	const baz = into_wire_Baz(_baz);
	let url = `/request_baz/`;
	const searchParams = new URLSearchParams();
	searchParams.set('str', str);

	searchParams.set('date', date);


	url = searchParams.size > 0 ? `${url}?${searchParams}` : url;
	let response = await fetch(url, {
		method: 'get',
		body: JSON.stringify({
			baz: baz
		}),
		headers: {
			'Content-Type': 'application/json'
		}
	});

	if(!response.ok)
		throw new Error(response.statusText);

	let j = await response.json();
	return j as Baz[]
}



