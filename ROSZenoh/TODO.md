# ROSZenoh TODO

## Per-message attachment (rmw_zenoh metadata)

The Zenoh.jl side is done — `Zenoh.serialize`/`deserialize` handle the framing.
Remaining here is the gid derivation and wiring.

- [x] **gid derivation** — `entity_gid(z_id, id)` (also `::NodeEntity` /
      `::EndpointEntity`) in `src/attachment.jl`. `sha256(z_id.to_le_bytes() ‖
      id_le_u64)[1:16]`, matching hiroz `endpoint_gid`. Note: `to_le_bytes`
      reverses zenoh's big-endian hex string back to the 16-byte LE array.
      Surfaces as `NTuple{16,UInt8}`.
- [x] **`encode_attachment(seq, ts, gid) -> ZBytes`** = `Zenoh.serialize((Int64,
      Int64, NTuple{16,UInt8}))`. In `src/attachment.jl`.
- [x] **`decode_attachment(sample) -> (seq, ts, gid)`** = `Zenoh.deserialize(
      Tuple{Int64,Int64,NTuple{16,UInt8}}, sample)`.
- [~] **Parity caveat:** gid is the **fixed `[u8;16]`** form (`NTuple{16,UInt8}`,
      no length prefix). Verified the wire is 32 bytes (8+8+16) and round-trips,
      but still **byte-diff against a real rmw_zenoh capture** to confirm the
      gid derivation (esp. the zid byte order) — this is the exactness pivot.

Wiring (consumed in ROSNode, but the encode/decode + gid live here):
- attach on publish / service request / reply; decode on receive into message-info.
- `(source_gid, sequence_number)` = the service request id (async reply correlation).
- `source_gid` also drives ignore-own-messages.

See `ROSNode/DESIGN.md` → "Per-message attachment" for the full rationale.

## QoS encoding completeness (deadline / lifespan / liveliness)

`encode_qos`/`decode_qos` (RmwZenoh) currently emit *placeholders* for the last
three QoS fields. Complete them to carry real values, byte-for-byte with
rmw_zenoh's QoS string.

Current format (`rmw_zenoh.jl`): `<rel>:<dur>:<hist>:,:,:,,` — i.e. deadline `,`,
lifespan `,`, liveliness `,,` are always empty/default.

- [ ] **`QosProfile` fields** — add `deadline::Union{Nothing,Duration}`,
      `lifespan::Union{Nothing,Duration}`, `liveliness::Symbol`
      (`:automatic`/`:manual_by_topic`), `liveliness_lease::Union{Nothing,Duration}`.
      `nothing` = ∞ (ROS2 default), `:automatic` default.
- [ ] **`encode_qos`** — fill the trailing fields: deadline `<sec>,<nsec>`,
      lifespan `<sec>,<nsec>`, liveliness `<kind>,<lease_sec>,<lease_nsec>`. Emit
      empty when default/∞ (match rmw_zenoh's "unset" representation exactly).
- [ ] **`decode_qos`** — parse them back; tolerate the empty/default forms (and
      older peers that omit them).
- [ ] **RxO compatibility** — extend the compat check (used by ROSNode's
      `on_qos_incompatible`) to deadline (offered ≤ requested) and liveliness
      (kind ordering + offered lease ≤ requested).
- [ ] **Parity:** byte-diff the QoS string against a real rmw_zenoh capture for
      each non-default policy — this is the exactness point, like the attachment.

See `ROSNode/DESIGN.md` → "QoS completeness & events" for how ROSNode consumes
these (compat check + the deadline/lifespan/liveliness event surface).
