# Built-in std_msgs mappers (DESIGN-MAPPING.md §5.2). Duck-typed on the `data` field.

map_std_string!(sink, m, ctx) = (emit!(sink, ctx, Rerun.Archetypes.TextLog([String(m.data)])); nothing)
map_std_scalar!(sink, m, ctx) = (emit!(sink, ctx, _scalars(m.data)); nothing)   # Bool/Int*/UInt*/Float*
