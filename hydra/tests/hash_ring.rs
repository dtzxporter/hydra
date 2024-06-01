use hydra::HashRing;

#[test]
fn hash_ring_works() {
    let ring: HashRing<u64> = HashRing::new();

    ring.add_node(0, 0xDEADBEEF);
    ring.add_node(1, 0xDEADC0DE);

    let node1 = ring.find_node(0);
    let node2 = ring.find_node(1);
    let node3 = ring.find_node(1337);

    assert_eq!(node1.unwrap(), 0xDEADBEEF);
    assert_eq!(node2.unwrap(), 0xDEADC0DE);

    assert!([0xDEADBEEF, 0xDEADC0DE].contains(&node3.unwrap()));
}

#[test]
fn hash_ring_set_nodes() {
    let ring: HashRing<u64> = HashRing::new();

    ring.add_node(0, 0xDEADBEEF);
    ring.add_node(1, 0xDEADC0DE);

    ring.set_nodes([(0, 0xDEADC0DE), (1, 0xDEADBEEF)]);

    let node1 = ring.find_node(0);
    let node2 = ring.find_node(1);

    assert_eq!(node1.unwrap(), 0xDEADC0DE);
    assert_eq!(node2.unwrap(), 0xDEADBEEF);
}

#[test]
fn hash_ring_clear() {
    let ring: HashRing<u64> = HashRing::new();

    ring.add_node(0, 0xDEADBEEF);
    ring.add_node(1, 0xDEADC0DE);

    assert!(ring.find_node(0).is_some());

    ring.clear();

    assert!(ring.find_node(0).is_none());
}

#[test]
fn hash_ring_overrides() {
    let ring: HashRing<u64> = HashRing::new();

    ring.add_node(0, 0xDEADBEEF);
    ring.add_node(1, 0xDEADC0DE);

    let node1 = ring.find_node(0);

    assert_eq!(node1.unwrap(), 0xDEADBEEF);

    ring.add_override(3, 0xC0DEBEEF);

    let node3 = ring.find_node(3);

    assert_eq!(node3.unwrap(), 0xC0DEBEEF);
}
