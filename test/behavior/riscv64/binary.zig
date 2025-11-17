const std = @import("std");
const math = std.math;
const cast = math.cast;

const Gpr = u64;

fn binary(comptime op: anytype, opts: struct { compare: Compare = .relaxed }) type {
    return struct {
        noinline fn testArgKinds(
            // TODO: support arguments spilling to the stack
            // _: Gpr,
            // _: Gpr,
            // _: Gpr,
            // _: Gpr,
            // _: Gpr,
            // _: Gpr,
            // _: Gpr,
            // _: Gpr,
            // TODO: add enough args here that it's spilled
            comptime Type: type,
            comptime imm_lhs: Type,
            mem_lhs: Type,
            comptime imm_rhs: Type,
            mem_rhs: Type,
        ) !void {
            const expected = comptime op(Type, imm_lhs, imm_rhs);
            var reg_lhs = mem_lhs;
            var reg_rhs = mem_rhs;
            _ = .{ &reg_lhs, &reg_rhs };

            try checkExpected(expected, op(Type, reg_lhs, reg_rhs), opts.compare);
            try checkExpected(expected, op(Type, reg_lhs, mem_rhs), opts.compare);
            try checkExpected(expected, op(Type, reg_lhs, imm_rhs), opts.compare);
            try checkExpected(expected, op(Type, mem_lhs, reg_rhs), opts.compare);
            try checkExpected(expected, op(Type, mem_lhs, mem_rhs), opts.compare);
            try checkExpected(expected, op(Type, mem_lhs, imm_rhs), opts.compare);
            try checkExpected(expected, op(Type, imm_lhs, reg_rhs), opts.compare);
            try checkExpected(expected, op(Type, imm_lhs, mem_rhs), opts.compare);
        }
        noinline fn testArgs(comptime Type: type, comptime imm_lhs: Type, comptime imm_rhs: Type) !void {
            try testArgKinds(
                // undefined,
                // undefined,
                // undefined,
                // undefined,
                // undefined,
                // undefined,
                // undefined,
                // undefined,
                // undefined,
                // undefined,
                // undefined,
                // undefined,
                // undefined,
                // undefined,
                // undefined,
                // undefined,
                Type,
                imm_lhs,
                imm_lhs,
                imm_rhs,
                imm_rhs,
            );
        }

        fn testBools() !void {
            try testArgs(bool, false, false);
            try testArgs(bool, false, true);
            try testArgs(bool, true, false);
            try testArgs(bool, true, true);
        }

        fn testInts() !void {
            try testArgs(i1, 0x0, -0x1);
            try testArgs(u1, 0x1, 0x1);
            try testArgs(i2, 0x0, -0x2);
            try testArgs(u2, 0x2, 0x1);
            try testArgs(i3, 0x1, -0x3);
            try testArgs(u3, 0x6, 0x1);
            try testArgs(i4, 0x6, 0x3);
            try testArgs(u4, 0x8, 0x5);
            try testArgs(i5, -0x9, -0xd);
            try testArgs(u5, 0x5, 0x13);
            try testArgs(i7, 0x34, 0x1d);
            try testArgs(u7, 0x31, 0x56);
            try testArgs(i8, -0x57, -0x70);
            try testArgs(u8, 0x12, 0xd6);
            try testArgs(i9, -0x8a, -0xa0);
            try testArgs(u9, 0xf8, 0x95);
            try testArgs(i15, -0x790, 0x116f);
            try testArgs(u15, 0x548b, 0x4cd6);
            try testArgs(i16, -0x2d17, -0x5c17);
            try testArgs(u16, 0xadc0, 0xb223);
            try testArgs(i17, 0xe543, 0xaad5);
            try testArgs(u17, 0x9515, 0xa3c1);
            try testArgs(i31, -0x28858a2f, 0x369e917a);
            try testArgs(u31, 0x32bab794, 0x75464e7f);
            try testArgs(i32, 0x79e74e44, 0x61fe4ab1);
            try testArgs(u32, 0xc82f8e2, 0x5dde37e2);
            try testArgs(i33, -0xa4cbaa13, -0x4d20ee61);
            try testArgs(u33, 0x17461d437, 0x16cbc228f);
            try testArgs(i63, -0x1, 0);
            try testArgs(i63, 0x333220e16b1e53fb, 0x121a0d970a5a4504);
            try testArgs(u63, 0x2dcd94e2ae4aa2af, 0x5f401e6e287a4dd7);
            // try testArgs(i64, 0x17e6bb7d8d430410, 0x760d42736f4b445c);
            // try testArgs(u64, 0x430970421452be50, 0xb4b5e96f4183b5fc);
            // try testArgs(i65, 0xb4477484679a6576, 0x21c9a3100d35de49);
            // try testArgs(u65, 0x1b7ffa914193a316, 0x6751268790308460);
            // try testArgs(i95, 0xd573e2100686f5df03aa29f, 0x4f7c921eb980b43a554b763);
            // try testArgs(u95, 0x62791162d2740f3ae84a9fcf, 0x1b6e66ae70bb9785a2118ecc);
            // try testArgs(i96, -0x6dc72375264ab887ea6073d5, 0x357ca705a600e94f6dd114c9);
            // try testArgs(u96, 0x77867877aae9bc90b2b57ce7, 0xd9a5352eb86061b67a61b212);
            // try testArgs(i97, 0x76f421e0ccfc6e7531c03ad5, 0x6775cdacdfca5455771c0dae);
            // try testArgs(u97, 0xaeb79499018e490b6aa2a5fc, 0x6cf53b08068cf25bdc307606);
            // try testArgs(i127, -0xc6de705251f892f8ba6a4f10aee0c7, -0x1598d3c6fd635ec0796a584af7479027);
            // try testArgs(u127, 0x5b3ec94f88a61621be2f745e90153390, 0x72456ad6a7ef886decf13195a50ca4d6);
            // try testArgs(i128, -0x44570544f745b89beb111016359577d5, -0x48904e59a05caede0974f916efba61a0);
            // try testArgs(u128, 0x3b14f670f6ac712d087a9ec7b15394d2, 0x19b69cb71a6763a9dc5baec5bb818450);
            // try testArgs(i129, 0xd58a765abb324106d83362db47fc374d, 0x548642028e222abf2ee21a1999a8ac5f);
            // try testArgs(u129, 0x1144fb18eba36e437bc45a73bbe25f10e, 0xdc7cb5f65f5127b00a842adf3f5a5231);
            // try testArgs(i159, 0x3121c6ae74c46679386f2051ee0520d9264e01cf, -0x34fec2cf28ce549281a5dc79f7ed834483f418af);
            // try testArgs(u159, 0x2e479684775f86a8ff1a9c6fab9022b18a6f6be4, 0x63c77ea3d97ad2c715fd13db972e678fefe3efba);
            // try testArgs(i160, 0x1e55924219aa114ef8d2b3193d09ae7849a3e551, -0x13f1ff6a62e562f7b78559f032bb05b2e2d15748);
            // try testArgs(u160, 0x8ed3d206fcc59350cf23dcd9e042eb36bcc63e52, 0xc88e1c5a42abf98aee0a3479e7f4fe88ab53b6f2);
            // try testArgs(i161, -0xd3d6885c0df36bd513aca744561684d12a62f044, 0xa519d3c4a7ea2e4768d840ec8641995689de6116);
            // try testArgs(u161, 0x10b4afdfa36471c77a2b629ef85e1289b798161b, 0x15e89da33c31ec01adf6921b8d13bc943f139fba2);
            // try testArgs(i191, 0x2436122b85c017733d9d28347544298d148223e1d9cbf0a2, 0x46b80688a0e0b59e66628940772893fcce258d3da7c0193);
            // try testArgs(u191, 0x3d00a8da821de44f98fa70d298bda9e25f99d8f54936d09f, 0x4ad4440686be966599985094f16e364c961503214ff86519);
            // try testArgs(i192, 0x124d0580271a71745f842e3a81d8cb6154c7f6f4b8b0cf39, -0x5bae9d7d471e609f1570a3f9805b80c4d672a086d44107eb);
            // try testArgs(u192, 0x3b882677dc62d5c76cc942bea0d2f72925ff0a9e234d7ce9, 0x5d7825e3f2254bf214257ebe84716dc88fde6c9563218ac4);
            // try testArgs(i193, 0x9c143db83d19c8fff1c23f3c93b103eaf8be02910a1cbe5, 0xcfa1059ba12508d2ff3ef9763ce8224eb1d0a0f22def289d);
            // try testArgs(u193, 0x635890f170da79117490445db595c1f2bb5a5cf640abc8e8, 0xdb5a2a6a3c6db7f43949123f0886cb93bbbed2d5dd7690e);
            // try testArgs(i223, -0x25e4a8d454e5957a9906a66a0c02ad53e727e3e18ca4b8be98561306, 0x8d6d3977afce56a5dffc537de19d4c73f2e5603699373d010e51d10);
            // try testArgs(u223, 0x5721636a3c6d271fe9eb08420d29454775666266801a7d23d61075be, 0x7e573fd8dcbd6dc780d13b61d5255cae790ea697d1c9a5479fa51ee);
            // try testArgs(i224, 0x51f97aaa96493aaed2677294bfde0715d69d961fef97a557ae9dbc84, 0x306d9305e2d5162dd0ce0454d2daaa54879b11a77386bb03e779a23e);
            // try testArgs(u224, 0xdc7eb2070c048b6fd22d6df97b3ef5e9fc5f28d8d229710333defecd, 0x475662887f29712bc927fae9de37cd842d883682a26e653d7b3f2ed9);
            // try testArgs(i225, -0x9b3dba4fe2026e8d90d9be4b8b2334034d2ae23569c4e1a3a311925d, -0x2ae4c074cff2da1e7fcab269ce6da7f4a9f763062f97526c0b4abf34);
            // try testArgs(u225, 0xb36dd536afb070e9be7fba5eaf548fe741182cabaf9f9510f86b3ffb, 0x1f35e5728f29e2566afd9a325beaf17ebf5f894e744825bdd56bb12d0);
            // try testArgs(i255, 0x2db696171e4045e17cb2a96763ff2728b459e5bf9ade6e9cd118bbc4f91aca89, -0x1580d80086052560091fe42077ce66c45d7e93173f74327f44fec7b63ed9f2aa);
            // try testArgs(u255, 0x392b3639141d03da49d576fd0ce498e1bafb8fc032604e68e91e589f6d2a05a3, 0x46f60e500f01bdbe18f71fb8dbef0395245a94f55421637ca50eb8922a751977);
            // try testArgs(i256, -0x2f3c22cb1d12628b2eccd705f1526d8a91258183742d9521bdc97d943591d87c, -0x864d3ef8b592e041289dbb54def60ceee798673138aa750a5efdaffcd42b62d);
            // try testArgs(u256, 0x25776f0ce5f3c6761eec99ace965f9162e9416e4d4e298674e5723b64e443528, 0xb1ee7fd2efaddd5d25eea49bde34e53c40d59221757f17d53d9a4c9ab7eca3f5);
            // try testArgs(i257, 0xd599706e1a09217f1f698520993d2b62ee877a4150bd8db6e5546657900dc7ce, 0x51d0faef82bb0878a4fd4331dcaec6ed57156acc2377c7e301eca6989e897346);
            // try testArgs(u257, 0xb0d42105facc0db629c5a65d6e975d25163841051efb1e187b70015f8c9e22ba, 0xf0eb6d0529e15fac6e97e850f50b7bf5056c9010345884926bf056590ddf3187);
            // try testArgs(i511, 0x23c07fa26fea1595de6e368cb42d05696562d8fb05a2aab6b304c443275071a31684a369f69f30fd53223017669dcf8157f7ff1bcda05ad28dcf46c92f7f2bd5, 0x44884ae45727d2c249b280cbb6795f237015f1082ade12167c52f0318422b3ae9c1753263011878e3fa4fce0db683efdd249e325188a40ccb959bd6bf050fbf);
            // try testArgs(u511, 0x642d98a41a7cc71dab7845c2c568696d0d77733c266846019756937cc29382d46074c8eb86502f4855c35f6354e51d98c41674166a9a7385ab94b0c7a63f58c0, 0x3cc5230f530a12c8cae29654e55a6d7cd26fe7606beed5c9a8fef443b107bf18dd8cc034683b47a213a3a885abd7048188713e8e7b9157145cd24748e256f5b7);
            // try testArgs(i512, -0x4b6d88a77e2a42d67daada905d16c6045b4dd57e608a0482f45531781d4994e2a6b71ad41a106b2dfc76e60aebd9e1d357b24b8d6889de3cf3e58ff3a48f54aa, -0x2e28e1b21ec33fd5dd9b1fbdc312e32884208b549ce0ca1661ca1150a6bd43363d4d186aa8ac70ad0595b44b5279ff070df8bd0b51095c62c8c499bfcaa7494c);
            // try testArgs(u512, 0xf40ad922664478a7b71a5676fc49434a45ba86975cef377c8321159cd880b67cd543fcca70187d5912675c0bc1fa4129cb470f280cde56ac4ec848ca589f143f, 0xe20a8110780ff05718adc173677ff0579126576f1fc3857ac41d6b7d5334d93134181af15ce2d35224d2e5c63384f33e331b16ecbc6db44edbb4074134d23e97);
            // try testArgs(i513, 0x55a556c6b6605897ffb7a791bdf309d5edb879f2841d1bba37006cdd0e7d00d971c85def024e28b7a17e53f3bcf5a5d5c43e780c6d13d67de1ca7b8f05deddfb, 0x53f475716443b38792e618ce109cec641aa351ce2e258a99153820c5522a4acc7f2b5b4ecd0000bcbe5a410bbee200576f6ff17ce7e8b7d1f0752390d1bb9b3f);
            // try testArgs(u513, 0x1e4e15bc406c558c14f48b83090647d7f2254fa571eac7f8aad8edb76a90547f7854bd6315e50ad44ea93034db9fab450a584b53abf8537e31d39cd706a31eaa7, 0x1d348de8124b72ca1d0e382501024c9e1b0f6fc16c5cd4a86aef2731bd39c29173749afe94bb2992ea805148fe0d96abdc5980b2143bd81419c1e40bf81b2496f);
            // try testArgs(i1023, -0x37781a6086d9310e4cdb24f5f374736e32af53c9545298aa53fe17854f73052cd808f658efacea622c59adb51af4d2dd636521ca2717acc43389c975505b7543da2c62f33c3152907f13b1ffa5b9881b33acec3cab1d8e33c2239ea6835277c474629a9157f8acd7c1d83076c2e75a48a8d3a94067e801c51057e47e09f0be14, 0x39ad04132dec8b795b98fd7cd085605ce8354655633068ee485d9dc78853feb922a54a3df6209989d1137e4ea8b0ad2cae48b21df2e0c04feeca56d2551f12782312a6ae483ffff466ff78446ebd4d47a61c1cba2603a62b44a72800060dda7eec8bc9060b8c5533afa7946bd38e93fddb863392500c22616dd4ae4932f20fe4);
            // try testArgs(u1023, 0x6e14a9d998a9ef7ac77b6fe08225fcc176e687685736e0e32c9e6b8fe96e9a7b14b3318310e945f7f84128455075eedb4a7b7736185f58e5640688c5d3b47d785338a0b70e77f4d237fd85f7820f3ebe6eb30f5a71231e813a70d6c76963d66291f271cd6f462ce685a0270ec5f6e856340f91d7597cd2b779566fe3ff4d4a98, 0x211eafbd449691e390d14dd34cb9c4d32627ecbec485d4a0a7cca1b28bd81d2153a7a75c2f62c4c3c7f198740cfe65dfa3c86156aa0b6d22757fd07f4070ddd50e334782f045a58b96c5a8f04b4615968501e9b5e801c475bdc034919c9e9df6df3cbbd59bacfa9409b21c3365d10e132d75958774c6244446127b043d155ec4);
            // try testArgs(i1024, 0xa6bd15fd4c529a24e4d727c5c0db9422ed7038ba23944b2b54ffc8d3731e2ffc19e12d885010fb10d208ea045e2a4cfec32d190f221bd453ec73fcecc03e37fd70be3fba3945c881cb9bce69f3f6ad9a6ee0d42a393b3669d1ca518d0b7b06a2c47978f22bc1db8802ef6ce29ea51b48256c6fed82e04355665f9d27ff485b, -0x69f258c2a86e97161ce2e683801591976a8e5a71b88605450961ee271637e6c2fee13459c29d42d4c5fb408f80236d3b2db34752e307fee6cbce2e5088adc817902a5adbbba72c8be84b9f8af5ce0fac464cae61cff9cbbe3a8dcdbec0af855b2e6c2c19fbe4f01baec9ca28b78bd7d383281c71d81da74fd0a2c8a5b754ee57);
            // try testArgs(u1024, 0x111e0a4e0c61ab3c5229154539ddb010542cb528533b4ea13813d8dbbaca7de395aac3c22dae1bd9db8bf9005ea9ef3df253aefabdce060a93e60da6edc3b1b2d78aead4647e7a589b66aa53fb953742b71f823b539150918df0fe781ee4d00279e4da9995804391bb19504de2f108f7d6ba14d624fa175842bd429f638de8f9, 0xcf9160dcb7ed13a738f7c8b2a17ca2fe84f53620a50f6a948698c4efca88392dc104ef5d26f19c82c8f770f727585702cc8d1c4cc2bba9e691e61b055d98cd636347a7c50b3bd2b2f5dfa416dadbdd76c111d45598c93ef729588cf998a55260cfe94d376ec4e8dc132afa42b66b68bc826c50169f9f4fc798cf7e8f29df639a);
            // try testArgs(i1025, 0xef2102c2cab6ad6bf2f2ba09c154440e65acc56cb14c5221a12b12404f7eafefeab4537f70cc10afb945e93c935223ffd3146911021666fd68fcaa494ded54ce66d2832b1d82b0654f24f1183bbc3ee45eb15c424a74ad41f22c7009b86cb404ac3b810445679417d7e0c5d5f4e88dec7c90352afa367004facbc1d668ab0a7, 0xc7743d3a52bad9bed0d24dbeaac4f27f4790ee14e984484f7ee077e6285394f046f2ba6d3a9c6e0aea1c07de98741a88669a035ec4d9755130fe96414223486e89d710a743ca2c2b53871fdb4851d90a595111d8d12e6732e4b580e235218edee3bc56fca3de99bc5f9a37c9dbc9a8ca5aaba710ec5e498b58b239a1be56915b);
            // try testArgs(u1025, 0x1dea81169800bac2f3afcf3be5dbd2d8eefbace8a24a2da0a383a928d1109459f34028be4413119f1af00ad90ce4d63064016dc1cee5b783c79c1998a0a49de21c4db71d432273576503589fc966c7ec2d730fa9bc4c5ff3128a82653ab8149528de67804718e39722f89b91c75d012ea41c642c889f0db95c882a9790a5e922f, 0x156fe02946ab9069a644dcc1f2b1afa04ee88ab1de19575a2715abf4a52bf374d297fdf78455ccdb87a934d3d818d774b63865eaedfdad3c56a56b8fcc62703c391aedf16cf770af06d7d205f93778c012df54fe5290084e1cd2bbec86a2f295cdce69a2cd774e064580f3c9cfae60d17b12f610e86566e68d5183d706c8ad8af);
        }
    };
}

pub fn ChangeScalar(comptime Type: type, comptime NewScalar: type) type {
    return switch (@typeInfo(Type)) {
        else => NewScalar,
        .vector => |vector| @Vector(vector.len, NewScalar),
    };
}

pub fn Scalar(comptime Type: type) type {
    return switch (@typeInfo(Type)) {
        else => Type,
        .vector => |info| info.child,
    };
}
pub fn AsSignedness(comptime Type: type, comptime signedness: std.builtin.Signedness) type {
    return switch (@typeInfo(Scalar(Type))) {
        .int => |int| ChangeScalar(Type, @Type(.{ .int = .{
            .signedness = signedness,
            .bits = int.bits,
        } })),
        .float => Type,
        else => @compileError(@typeName(Type)),
    };
}
pub fn AddOneBit(comptime Type: type) type {
    return ChangeScalar(Type, switch (@typeInfo(Scalar(Type))) {
        .int => |int| @Type(.{ .int = .{ .signedness = int.signedness, .bits = 1 + int.bits } }),
        .float => Scalar(Type),
        else => @compileError(@typeName(Type)),
    });
}
pub fn DoubleBits(comptime Type: type) type {
    return ChangeScalar(Type, switch (@typeInfo(Scalar(Type))) {
        .int => |int| @Type(.{ .int = .{ .signedness = int.signedness, .bits = int.bits * 2 } }),
        .float => Scalar(Type),
        else => @compileError(@typeName(Type)),
    });
}
pub fn Log2Int(comptime Type: type) type {
    return ChangeScalar(Type, math.Log2Int(Scalar(Type)));
}

pub fn splat(comptime Type: type, scalar: Scalar(Type)) Type {
    return switch (@typeInfo(Type)) {
        else => scalar,
        .vector => @splat(scalar),
    };
}

const Compare = enum { strict, relaxed, approx, approx_int, approx_or_overflow };
noinline fn checkExpected(expected: anytype, actual: @TypeOf(expected), comptime compare: Compare) !void {
    const Expected = @TypeOf(expected);
    const unexpected = switch (@typeInfo(Scalar(Expected))) {
        else => expected != actual,
        .float => @compileError("TODO"),
        .@"struct" => |@"struct"| inline for (@"struct".fields) |field| {
            try checkExpected(@field(expected, field.name), @field(actual, field.name), compare);
        } else return,
    };
    if (switch (@typeInfo(Expected)) {
        else => unexpected,
        .vector => @reduce(.Or, unexpected),
    }) return error.Unexpected;
}

inline fn equal(comptime Type: type, lhs: Type, rhs: Type) ChangeScalar(Type, bool) {
    return lhs == rhs;
}
test equal {
    const test_equal = binary(equal, .{});
    try test_equal.testInts();
}

inline fn notEqual(comptime Type: type, lhs: Type, rhs: Type) ChangeScalar(Type, bool) {
    return lhs != rhs;
}
test notEqual {
    const test_not_equal = binary(notEqual, .{});
    try test_not_equal.testInts();
}

inline fn lessThan(comptime Type: type, lhs: Type, rhs: Type) ChangeScalar(Type, bool) {
    return lhs < rhs;
}
test lessThan {
    const test_less_than = binary(lessThan, .{});
    try test_less_than.testInts();
}

inline fn lessThanEqual(comptime Type: type, lhs: Type, rhs: Type) ChangeScalar(Type, bool) {
    return lhs <= rhs;
}
test lessThanEqual {
    const test_less_than = binary(lessThanEqual, .{});
    try test_less_than.testInts();
}

inline fn greaterThan(comptime Type: type, lhs: Type, rhs: Type) ChangeScalar(Type, bool) {
    return lhs > rhs;
}
test greaterThan {
    const test_less_than = binary(greaterThan, .{});
    try test_less_than.testInts();
}

inline fn greatThanEqual(comptime Type: type, lhs: Type, rhs: Type) ChangeScalar(Type, bool) {
    return lhs >= rhs;
}
test greatThanEqual {
    const test_less_than = binary(greatThanEqual, .{});
    try test_less_than.testInts();
}

inline fn addUnsafe(comptime Type: type, lhs: Type, rhs: Type) AddOneBit(Type) {
    @setRuntimeSafety(false);
    return @as(AddOneBit(Type), lhs) + rhs;
}
test addUnsafe {
    const test_add_unsafe = binary(addUnsafe, .{});
    try test_add_unsafe.testInts();
}

inline fn addWrap(comptime Type: type, lhs: Type, rhs: Type) Type {
    return lhs +% rhs;
}
test addWrap {
    const test_add_wrap = binary(addWrap, .{});
    try test_add_wrap.testInts();
}

// // inline fn addWithOverflow(comptime Type: type, lhs: Type, rhs: Type) struct { Type, ChangeScalar(Type, u1) } {
// //     return @addWithOverflow(lhs, rhs);
// // }
// // test addWithOverflow {
// //     const test_add_with_overflow = binary(addWithOverflow, .{});
// //     try test_add_with_overflow.testInts();
// // }

inline fn max(comptime Type: type, lhs: Type, rhs: Type) Type {
    return @max(lhs, rhs);
}
test max {
    const test_max = binary(max, .{});
    try test_max.testInts();
}

inline fn min(comptime Type: type, lhs: Type, rhs: Type) Type {
    return @min(lhs, rhs);
}
test min {
    const test_min = binary(min, .{});
    try test_min.testInts();
}

// inline fn subUnsafe(comptime Type: type, lhs: Type, rhs: Type) AddOneBit(Type) {
//     @setRuntimeSafety(false);
//     return switch (@typeInfo(Scalar(Type))) {
//         else => @compileError(@typeName(Type)),
//         .int => |int| switch (int.signedness) {
//             .signed => @as(AddOneBit(Type), lhs) - rhs,
//             .unsigned => @as(AddOneBit(Type), @max(lhs, rhs)) - @min(lhs, rhs),
//         },
//         .float => lhs - rhs,
//     };
// }
// test subUnsafe {
//     const test_sub_unsafe = binary(subUnsafe, .{});
//     try test_sub_unsafe.testInts();
// }

inline fn subWrap(comptime Type: type, lhs: Type, rhs: Type) Type {
    return lhs -% rhs;
}
test subWrap {
    const test_add_wrap = binary(subWrap, .{});
    try test_add_wrap.testInts();
}

// // inline fn subWithOverflow(comptime Type: type, lhs: Type, rhs: Type) struct { Type, ChangeScalar(Type, u1) } {
// //     return @subWithOverflow(lhs, rhs);
// // }
// // test subWithOverflow {
// //     const test_add_with_overflow = binary(subWithOverflow, .{});
// //     try test_add_with_overflow.testInts();
// // }

// inline fn bitAnd(comptime Type: type, lhs: Type, rhs: Type) Type {
//     return lhs & rhs;
// }
// test bitAnd {
//     const test_bit_and = binary(bitAnd, .{});
//     try test_bit_and.testBools();
//     try test_bit_and.testInts();
// }

// inline fn bitOr(comptime Type: type, lhs: Type, rhs: Type) Type {
//     return lhs | rhs;
// }
// test bitOr {
//     const test_bit_or = binary(bitOr, .{});
//     try test_bit_or.testBools();
//     try test_bit_or.testInts();
// }

// inline fn bitXor(comptime Type: type, lhs: Type, rhs: Type) Type {
//     return lhs ^ rhs;
// }
// test bitXor {
//     const test_bit_xor = binary(bitXor, .{});
//     try test_bit_xor.testBools();
//     try test_bit_xor.testInts();
// }
