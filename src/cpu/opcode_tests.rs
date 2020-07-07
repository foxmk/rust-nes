use crate::cpu::test_helpers::*;
use crate::cpu::test_helpers::Register::*;
use crate::cpu::Flag::*;

mod lda {
    use super::*;

    mod imm {
        use super::*;

        #[test]
        fn should_set_register_to_imm_value() {
            run(&[0xA9, 0x01])
                .step()
                .assert_reg(A, 0x01);
        }

        #[test]
        fn should_take_2_cycles() {
            run(&[0xA9, 0x01])
                .step()
                .assert_cycles(2);
        }

        #[test]
        fn should_set_negative_flag() {
            run(&[0xA9, NEG_NUMBER]).with_flag(N, false)
                .step()
                .assert_flag(N, true);
        }

        #[test]
        fn should_clear_negative_flag() {
            run(&[0xA9, POS_NUMBER]).with_flag(N, true)
                .step()
                .assert_flag(N, false);
        }

        #[test]
        fn should_set_zero_flag() {
            run(&[0xA9, ZERO]).with_flag(Z, false)
                .step()
                .assert_flag(Z, true);
        }

        #[test]
        fn should_clear_zero_flag() {
            run(&[0xA9, NON_ZERO]).with_flag(Z, true)
                .step()
                .assert_flag(Z, false);
        }
    }

    mod abs {
        use super::*;

        #[test]
        fn should_load_value_from_addr() {
            run(&[0xAD, 0xFE, 0xCA])
                .with_mem(0xCAFE, &[0xFF])
                .step()
                .assert_reg(A, 0xFF);
        }

        #[test]
        fn should_take_4_cycles() {
            run(&[0xAD, 0xFE, 0xCA])
                .with_mem(0xCAFE, &[0xFF])
                .step()
                .assert_cycles(4);
        }

        #[test]
        fn should_set_negative_flag() {
            run(&[0xAD, 0xFE, 0xCA]).with_flag(N, false)
                .with_mem(0xCAFE, &[NEG_NUMBER])
                .step()
                .assert_flag(N, true);
        }

        #[test]
        fn should_clear_negative_flag() {
            run(&[0xAD, 0xFE, 0xCA]).with_flag(N, true)
                .with_mem(0xCAFE, &[POS_NUMBER])
                .step()
                .assert_flag(N, false);
        }

        #[test]
        fn should_set_zero_flag() {
            run(&[0xAD, 0xFE, 0xCA]).with_flag(Z, false)
                .with_mem(0xCAFE, &[ZERO])
                .step()
                .assert_flag(Z, true);
        }

        #[test]
        fn should_clear_zero_flag() {
            run(&[0xAD, 0xFE, 0xCA]).with_flag(Z, true)
                .with_mem(0xCAFE, &[NON_ZERO])
                .step()
                .assert_flag(Z, false);
        }
    }

    mod abs_x {
        use super::*;

        #[test]
        fn should_load_a_from_addr_indexed_by_x() {
            run(&[0xBD, 0x20, 0x10]).with_reg(X, 0x12)
                .with_mem(0x1020 + 0x12, &[0xFF])
                .step()
                .assert_reg(A, 0xFF);
        }

        #[test]
        fn should_take_4_cycles() {
            run(&[0xBD, 0x20, 0x10]).with_reg(X, 0x12)
                .with_mem(0x1020 + 0x12, &[0xFF])
                .step()
                .assert_cycles(4);
        }

        #[test]
        fn should_set_negative_flag() {
            run(&[0xBD, 0x20, 0x10]).with_reg(X, 0x12).with_flag(N, false)
                .with_mem(0x1020 + 0x12, &[NEG_NUMBER])
                .step()
                .assert_flag(N, true);
        }

        #[test]
        fn should_clear_negative_flag() {
            run(&[0xBD, 0x20, 0x10]).with_reg(X, 0x12).with_flag(N, true)
                .with_mem(0x1020 + 0x12, &[POS_NUMBER])
                .step()
                .assert_flag(N, false);
        }

        #[test]
        fn should_set_zero_flag() {
            run(&[0xBD, 0x20, 0x10]).with_reg(X, 0x12).with_flag(Z, false)
                .with_mem(0x1020 + 0x12, &[ZERO])
                .step()
                .assert_flag(Z, true);
        }

        #[test]
        fn should_clear_zero_flag() {
            run(&[0xBD, 0x20, 0x10]).with_reg(X, 0x12).with_flag(Z, true)
                .with_mem(0x1020 + 0x12, &[NON_ZERO])
                .step()
                .assert_flag(Z, false);
        }

        #[test]
        fn should_add_cycle_on_page_cross() {
            run(&[0xBD, 0xFF, 0x21]).with_reg(X, 0x01)
                .with_mem(0x21FF + 0x01, &[0xAD])
                .step()
                .assert_reg(A, 0xAD);
        }
    }

    mod abs_y {
        use super::*;

        #[test]
        fn should_load_a_from_addr_indexed_by_y() {
            run(&[0xB9, 0x20, 0x10]).with_reg(Y, 0x12)
                .with_mem(0x1020 + 0x0012, &[0xFF])
                .step()
                .assert_reg(A, 0xFF);
        }

        #[test]
        fn should_take_4_cycles() {
            run(&[0xB9, 0x20, 0x10]).with_reg(Y, 0x12)
                .with_mem(0x1020 + 0x0012, &[0xFF])
                .step()
                .assert_cycles(4);
        }

        #[test]
        fn should_set_negative_flag() {
            run(&[0xB9, 0x20, 0x10]).with_reg(Y, 0x12).with_flag(N, false)
                .with_mem(0x1020 + 0x12, &[NEG_NUMBER])
                .step()
                .assert_flag(N, true);
        }

        #[test]
        fn should_clear_negative_flag() {
            run(&[0xB9, 0x20, 0x10]).with_reg(Y, 0x12).with_flag(N, true)
                .with_mem(0x1020 + 0x12, &[POS_NUMBER])
                .step()
                .assert_flag(N, false);
        }

        #[test]
        fn should_set_zero_flag() {
            run(&[0xB9, 0x20, 0x10]).with_flag(Z, false).with_reg(Y, 0x12)
                .with_mem(0x1020 + 0x12, &[ZERO])
                .step()
                .assert_flag(Z, true);
        }

        #[test]
        fn should_clear_zero_flag() {
            run(&[0xB9, 0x20, 0x10]).with_flag(Z, true).with_reg(Y, 0x12)
                .with_mem(0x1020 + 0x12, &[NON_ZERO])
                .step()
                .assert_flag(Z, false);
        }

        #[test]
        fn should_add_cycle_on_page_cross() {
            run(&[0xB9, 0xFF, 0x21]).with_reg(Y, 0x01)
                .with_mem(0x21FF + 0x01, &[0xAD])
                .step()
                .assert_cycles(5);
        }
    }

    mod zpg {
        use super::*;

        #[test]
        fn should_load_a_from_zpg_addr() {
            run(&[0xA5, 0xED]).with_mem(0x00ED, &[0xFF])
                .step()
                .assert_reg(A, 0xFF);
        }

        #[test]
        fn should_take_3_cycles() {
            run(&[0xA5, 0xED]).with_mem(0x00ED, &[0xFF])
                .step()
                .assert_cycles(3);
        }

        #[test]
        fn should_set_negative_flag() {
            run(&[0xA5, 0xED]).with_flag(N, false)
                .with_mem(0x00ED, &[NEG_NUMBER])
                .step()
                .assert_flag(N, true);
        }

        #[test]
        fn should_clear_negative_flag() {
            run(&[0xA5, 0xED]).with_flag(N, true)
                .with_mem(0x00ED, &[POS_NUMBER])
                .step()
                .assert_flag(N, false);
        }

        #[test]
        fn should_set_zero_flag() {
            run(&[0xA5, 0xED]).with_flag(Z, false)
                .with_mem(0x00ED, &[ZERO])
                .step()
                .assert_flag(Z, true);
        }

        #[test]
        fn should_clear_zero_flag() {
            run(&[0xA5, 0xED]).with_flag(Z, true)
                .with_mem(0x00ED, &[NON_ZERO])
                .step()
                .assert_flag(Z, false);
        }
    }

    mod zpg_x {
        use super::*;

        #[test]
        fn should_load_a_from_addr_indexed_by_x() {
            run(&[0xB5, 0xED]).with_reg(X, 0x11)
                .with_mem(0x00ED + 0x11, &[0xFF])
                .step()
                .assert_reg(A, 0xFF);
        }

        #[test]
        fn should_take_4_cycles() {
            run(&[0xB5, 0xED]).with_reg(X, 0x11)
                .with_mem(0x00ED + 0x11, &[0xFF])
                .step()
                .assert_cycles(4);
        }

        #[test]
        fn should_set_negative_flag() {
            run(&[0xB5, 0xED]).with_flag(N, false).with_reg(X, 0x11)
                .with_mem(0x00ED + 0x11, &[NEG_NUMBER])
                .step()
                .assert_flag(N, true);
        }

        #[test]
        fn should_clear_negative_flag() {
            run(&[0xB5, 0xED]).with_flag(N, true).with_reg(X, 0x11)
                .with_mem(0x00ED + 0x11, &[POS_NUMBER])
                .step()
                .assert_flag(N, false);
        }

        #[test]
        fn should_set_zero_flag() {
            run(&[0xB5, 0xED]).with_reg(X, 0x11).with_flag(Z, false)
                .with_mem(0x00ED + 0x11, &[ZERO])
                .step()
                .assert_flag(Z, true);
        }

        #[test]
        fn should_clear_zero_flag() {
            run(&[0xB5, 0xED]).with_reg(X, 0x11).with_flag(Z, true)
                .with_mem(0x00ED + 0x11, &[NON_ZERO])
                .step()
                .assert_flag(Z, false);
        }
    }

    mod x_ind {
        use super::*;

        #[test]
        fn should_load_a_from_addr_pointed_by_indirect_pre_indexed_by_x() {
            run(&[0xA1, 0xED]).with_reg(X, 0x01)
                .with_mem(0x00ED + 0x01, &[0xFE, 0xCA]).with_mem(0xCAFE, &[0xFF])
                .step()
                .assert_reg(A, 0xFF);
        }

        #[test]
        fn should_take_6_cycles() {
            run(&[0xA1, 0xED]).with_reg(X, 0x01)
                .with_mem(0x00ED + 0x01, &[0xFE, 0xCA]).with_mem(0xCAFE, &[0xFF])
                .step()
                .assert_cycles(6);
        }

        #[ignore]
        #[test]
        fn should_not_overflow() {
            run(&[0xA1, 0xFF]).with_reg(X, 0xFF)
                .with_mem(0x00FF + 0xFF, &[0xFE, 0xCA]).with_mem(0xCAFE, &[0xFF])
                .step()
                .assert_reg(A, 0xFF);
        }

        #[test]
        fn should_clear_negative_flag() {
            run(&[0xA1, 0xED]).with_reg(X, 0x01).with_flag(N, true)
                .with_mem(0x00ED + 0x01, &[0xFE, 0xCA]).with_mem(0xCAFE, &[POS_NUMBER])
                .step()
                .assert_flag(N, false);
        }

        #[test]
        fn should_set_negative_flag() {
            run(&[0xA1, 0xED]).with_reg(X, 0x01).with_flag(N, false)
                .with_mem(0x00ED + 0x01, &[0xFE, 0xCA]).with_mem(0xCAFE, &[NEG_NUMBER])
                .step()
                .assert_flag(N, true);
        }

        #[test]
        fn should_set_zero_flag() {
            run(&[0xA1, 0xED]).with_reg(X, 0x01).with_flag(Z, false)
                .with_mem(0x00ED + 0x01, &[0xFE, 0xCA]).with_mem(0xCAFE, &[ZERO])
                .step()
                .assert_flag(Z, true);
        }

        #[test]
        fn should_clear_zero_flag() {
            run(&[0xA1, 0xED]).with_reg(X, 0x01).with_flag(Z, true)
                .with_mem(0x00ED + 0x01, &[0xFE, 0xCA]).with_mem(0xCAFE, &[NON_ZERO])
                .step()
                .assert_flag(Z, false);
        }
    }

    mod ind_y {
        use super::*;

        #[test]
        fn should_load_a_from_addr_pointed_by_indirect_post_indexed_by_y() {
            run(&[0xB1, 0xED]).with_reg(Y, 0x01)
                .with_mem(0x00ED, &[0xFE, 0xCA]).with_mem(0xCAFE + 0x01, &[0xFF])
                .step()
                .assert_reg(A, 0xFF);
        }

        #[test]
        fn should_take_5_cycles() {
            run(&[0xB1, 0xED]).with_reg(Y, 0x01)
                .with_mem(0x00ED, &[0xFE, 0xCA]).with_mem(0xCAFE + 0x01, &[0xFF])
                .step()
                .assert_cycles(5);
        }

        #[ignore]
        #[test]
        fn should_overflow() {
            run(&[0xB1, 0xFF]).with_reg(Y, 0xFF)
                .with_mem(0x00FF, &[0xFE, 0xCA]).with_mem(0xCAFE + 0xFF, &[0xFF])
                .step()
                .assert_reg(A, 0xFF);
        }

        #[test]
        fn should_set_negative_flag() {
            run(&[0xB1, 0xED]).with_reg(Y, 0x01)
                .with_flag(N, true).with_mem(0x00ED, &[0xFE, 0xCA]).with_mem(0xCAFE + 0x01, &[POS_NUMBER])
                .step()
                .assert_flag(N, false);
        }

        #[test]
        fn should_clear_negative_flag() {
            run(&[0xB1, 0xED]).with_reg(Y, 0x01).with_flag(N, false)
                .with_mem(0x00ED, &[0xFE, 0xCA]).with_mem(0xCAFE + 0x01, &[NEG_NUMBER])
                .step()
                .assert_flag(N, true);
        }

        #[test]
        fn should_set_zero_flag() {
            run(&[0xB1, 0xED]).with_reg(Y, 0x01).with_flag(Z, false)
                .with_mem(0x00ED, &[0xFE, 0xCA]).with_mem(0xCAFE + 0x01, &[ZERO])
                .step()
                .assert_flag(Z, true);
        }

        #[test]
        fn should_clear_zero_flag() {
            run(&[0xB1, 0xED]).with_reg(Y, 0x01).with_flag(Z, true)
                .with_mem(0x00ED, &[0xFE, 0xCA]).with_mem(0xCAFE + 0x01, &[NON_ZERO])
                .step()
                .assert_flag(Z, false);
        }
    }
}

mod ldx {
    use super::*;

    mod imm {
        use super::*;

        #[test]
        fn should_set_register_to_imm_value() {
            run(&[0xA2, 0x01])
                .step()
                .assert_reg(X, 0x01);
        }

        #[test]
        fn should_take_2_cycles() {
            run(&[0xA2, 0x01])
                .step()
                .assert_cycles(2);
        }

        #[test]
        fn should_set_negative_flag() {
            run(&[0xA2, NEG_NUMBER]).with_flag(N, false)
                .step()
                .assert_flag(N, true);
        }

        #[test]
        fn should_clear_negative_flag() {
            run(&[0xA2, POS_NUMBER]).with_flag(N, true)
                .step()
                .assert_flag(N, false);
        }

        #[test]
        fn should_set_zero_flag() {
            run(&[0xA2, ZERO]).with_flag(Z, false)
                .step()
                .assert_flag(Z, true);
        }

        #[test]
        fn should_clear_zero_flag() {
            run(&[0xA2, NON_ZERO]).with_flag(Z, true)
                .step()
                .assert_flag(Z, false);
        }
    }

    mod abs {
        use super::*;

        #[test]
        fn should_load_value_from_addr() {
            run(&[0xAE, 0xFE, 0xCA])
                .with_mem(0xCAFE, &[0xFF])
                .step()
                .assert_reg(X, 0xFF);
        }

        #[test]
        fn should_take_4_cycles() {
            run(&[0xAE, 0xFE, 0xCA])
                .with_mem(0xCAFE, &[0xFF])
                .step()
                .assert_cycles(4);
        }

        #[test]
        fn should_set_negative_flag() {
            run(&[0xAE, 0xFE, 0xCA]).with_flag(N, false)
                .with_mem(0xCAFE, &[NEG_NUMBER])
                .step()
                .assert_flag(N, true);
        }

        #[test]
        fn should_clear_negative_flag() {
            run(&[0xAE, 0xFE, 0xCA]).with_flag(N, true)
                .with_mem(0xCAFE, &[POS_NUMBER])
                .step()
                .assert_flag(N, false);
        }

        #[test]
        fn should_set_zero_flag() {
            run(&[0xAE, 0xFE, 0xCA]).with_flag(Z, false)
                .with_mem(0xCAFE, &[ZERO])
                .step()
                .assert_flag(Z, true);
        }

        #[test]
        fn should_clear_zero_flag() {
            run(&[0xAE, 0xFE, 0xCA]).with_flag(Z, true)
                .with_mem(0xCAFE, &[NON_ZERO])
                .step()
                .assert_flag(Z, false);
        }
    }

    mod abs_y {
        use super::*;

        #[test]
        fn should_load_a_from_addr_indexed_by_y() {
            run(&[0xBE, 0x20, 0x10]).with_reg(Y, 0x12)
                .with_mem(0x1020 + 0x0012, &[0xFF])
                .step()
                .assert_reg(X, 0xFF);
        }

        #[test]
        fn should_take_4_cycles() {
            run(&[0xBE, 0x20, 0x10]).with_reg(Y, 0x12)
                .with_mem(0x1020 + 0x0012, &[0xFF])
                .step()
                .assert_cycles(4);
        }

        #[test]
        fn should_set_negative_flag() {
            run(&[0xBE, 0x20, 0x10]).with_reg(Y, 0x12).with_flag(N, false)
                .with_mem(0x1020 + 0x12, &[NEG_NUMBER])
                .step()
                .assert_flag(N, true);
        }

        #[test]
        fn should_clear_negative_flag() {
            run(&[0xBE, 0x20, 0x10]).with_reg(Y, 0x12).with_flag(N, true)
                .with_mem(0x1020 + 0x12, &[POS_NUMBER])
                .step()
                .assert_flag(N, false);
        }

        #[test]
        fn should_set_zero_flag() {
            run(&[0xBE, 0x20, 0x10]).with_flag(Z, false).with_reg(Y, 0x12)
                .with_mem(0x1020 + 0x12, &[ZERO])
                .step()
                .assert_flag(Z, true);
        }

        #[test]
        fn should_clear_zero_flag() {
            run(&[0xBE, 0x20, 0x10]).with_flag(Z, true).with_reg(Y, 0x12)
                .with_mem(0x1020 + 0x12, &[NON_ZERO])
                .step()
                .assert_flag(Z, false);
        }

        #[test]
        fn should_add_cycle_on_page_cross() {
            run(&[0xBE, 0xFF, 0x21]).with_reg(Y, 0x01)
                .with_mem(0x21FF + 0x01, &[0xAD])
                .step()
                .assert_cycles(5);
        }
    }

    mod zpg {
        use super::*;

        #[test]
        fn should_load_a_from_zpg_addr() {
            run(&[0xA6, 0xED]).with_mem(0x00ED, &[0xFF])
                .step()
                .assert_reg(X, 0xFF);
        }

        #[test]
        fn should_take_3_cycles() {
            run(&[0xA6, 0xED]).with_mem(0x00ED, &[0xFF])
                .step()
                .assert_cycles(3);
        }

        #[test]
        fn should_set_negative_flag() {
            run(&[0xA6, 0xED]).with_flag(N, false)
                .with_mem(0x00ED, &[NEG_NUMBER])
                .step()
                .assert_flag(N, true);
        }

        #[test]
        fn should_clear_negative_flag() {
            run(&[0xA6, 0xED]).with_flag(N, true)
                .with_mem(0x00ED, &[POS_NUMBER])
                .step()
                .assert_flag(N, false);
        }

        #[test]
        fn should_set_zero_flag() {
            run(&[0xA6, 0xED]).with_flag(Z, false)
                .with_mem(0x00ED, &[ZERO])
                .step()
                .assert_flag(Z, true);
        }

        #[test]
        fn should_clear_zero_flag() {
            run(&[0xA6, 0xED]).with_flag(Z, true)
                .with_mem(0x00ED, &[NON_ZERO])
                .step()
                .assert_flag(Z, false);
        }
    }

    mod zpg_y {
        use super::*;

        #[test]
        fn should_load_a_from_addr_indexed_by_y() {
            run(&[0xB6, 0xED]).with_reg(Y, 0x11)
                .with_mem(0x00ED + 0x11, &[0xFF])
                .step()
                .assert_reg(X, 0xFF);
        }

        #[test]
        fn should_take_4_cycles() {
            run(&[0xB6, 0xED]).with_reg(Y, 0x11)
                .with_mem(0x00ED + 0x11, &[0xFF])
                .step()
                .assert_cycles(4);
        }

        #[test]
        fn should_set_negative_flag() {
            run(&[0xB6, 0xED]).with_flag(N, false).with_reg(Y, 0x11)
                .with_mem(0x00ED + 0x11, &[NEG_NUMBER])
                .step()
                .assert_flag(N, true);
        }

        #[test]
        fn should_clear_negative_flag() {
            run(&[0xB6, 0xED]).with_flag(N, true).with_reg(Y, 0x11)
                .with_mem(0x00ED + 0x11, &[POS_NUMBER])
                .step()
                .assert_flag(N, false);
        }

        #[test]
        fn should_set_zero_flag() {
            run(&[0xB6, 0xED]).with_reg(Y, 0x11).with_flag(Z, false)
                .with_mem(0x00ED + 0x11, &[ZERO])
                .step()
                .assert_flag(Z, true);
        }

        #[test]
        fn should_clear_zero_flag() {
            run(&[0xB6, 0xED]).with_reg(Y, 0x11).with_flag(Z, true)
                .with_mem(0x00ED + 0x11, &[NON_ZERO])
                .step()
                .assert_flag(Z, false);
        }
    }
}
