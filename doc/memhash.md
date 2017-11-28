

# Module memhash #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-memhash">memhash()</a> ###


__abstract datatype__: `memhash()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#destroy-1">destroy/1</a></td><td></td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_all-1">get_all/1</a></td><td></td></tr><tr><td valign="top"><a href="#keys-1">keys/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td></td></tr><tr><td valign="top"><a href="#remove-2">remove/2</a></td><td></td></tr><tr><td valign="top"><a href="#rget-2">rget/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_id-3">set_id/3</a></td><td>set a reference inside of the memhash.</td></tr><tr><td valign="top"><a href="#set_val-3">set_val/3</a></td><td>set a value inside of the memhash with that value.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="destroy-1"></a>

### destroy/1 ###

<pre><code>
destroy(Ref::<a href="#type-memhash">memhash()</a>) -&gt; ok
</code></pre>
<br />

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(Ref::<a href="#type-memhash">memhash()</a>, Key::term()) -&gt; term()
</code></pre>
<br />

<a name="get_all-1"></a>

### get_all/1 ###

<pre><code>
get_all(Ref::<a href="#type-memhash">memhash()</a>) -&gt; [term()]
</code></pre>
<br />

<a name="keys-1"></a>

### keys/1 ###

<pre><code>
keys(Ref::<a href="#type-memhash">memhash()</a>) -&gt; [term()]
</code></pre>
<br />

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-memhash">memhash()</a>
</code></pre>
<br />

<a name="remove-2"></a>

### remove/2 ###

<pre><code>
remove(Ref::<a href="#type-memhash">memhash()</a>, Key::term()) -&gt; ok
</code></pre>
<br />

<a name="rget-2"></a>

### rget/2 ###

<pre><code>
rget(Ref::<a href="#type-memhash">memhash()</a>, Key::term()) -&gt; term()
</code></pre>
<br />

<a name="set_id-3"></a>

### set_id/3 ###

<pre><code>
set_id(Ref::<a href="#type-memhash">memhash()</a>, Key::term(), ID::<a href="memhash_data.md#type-table_id">memhash_data:table_id()</a>) -&gt; ok
</code></pre>
<br />

set a reference inside of the memhash. The reference must be an
ID from other part of the memhash (same memhash).

<a name="set_val-3"></a>

### set_val/3 ###

<pre><code>
set_val(Ref::<a href="#type-memhash">memhash()</a>, Key::term(), Value::term()) -&gt; ok
</code></pre>
<br />

set a value inside of the memhash with that value. It's added
to the datatable if the key doesn't exist or replace the value
in the table otherwise.

