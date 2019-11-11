#include <iostream>
#include <algorithm>
#include <vector>
#include <bls.hpp>
#include <stdlib.h>
#include <memory>
#include <chrono>
#include <assert.h>

using namespace std;

void
print_ins_signature(bls::InsecureSignature const& sig)
{
    uint8_t sigBytes[bls::InsecureSignature::SIGNATURE_SIZE];
    sig.Serialize(sigBytes);
    for (auto b: sigBytes) cout << "0x" << hex << (int)b << " ";
    cout << dec;
}

void
test_thresh(size_t const T, size_t const N)
{
    vector<vector<bls::PublicKey>>  p_commitments(N);
    vector<vector<bls::PrivateKey>> p_secretFragments(N);
    vector<bls::PrivateKey>         p_secretKeys;
    for(size_t player = 0; player < N; ++player)
        for(size_t i = 0; i < N; ++i)
        {
            g1_t g;
            p_commitments[player].emplace_back(bls::PublicKey::FromG1(&g));
            bn_t b;
            bn_new(b);
            p_secretFragments[player].emplace_back(bls::PrivateKey::FromBN(b));
        }

    // Каждый участник создаёт набор фрагментов
    for(size_t player = 0; player < N; ++player)
    {
        auto const player_secret_key =
            bls::Threshold::Create(p_commitments[player], p_secretFragments[player], T, N);
        p_secretKeys.push_back(player_secret_key);
    }

    // Участники обмениваются друг с другом секретными фрагментами
    // и проверяют их
    for(size_t player_source = 1; player_source <= N; ++player_source) // NB: смотри индексы!
        for(size_t player_target = 1; player_target <= N; ++player_target) // NB: смотри индексы!
            assert(bls::Threshold::VerifySecretFragment(
                        player_target,
                        p_secretFragments[player_source-1][player_target-1], p_commitments[player_source-1], T));


    // Каждый участник вычисляет публичный ключ
    std::vector<bls::PublicKey> preMasterPubKey;
    for(size_t i = 0; i < N; ++i) preMasterPubKey.emplace_back(p_commitments[i][0]);
    bls::PublicKey masterPubkey = bls::PublicKey::AggregateInsecure(preMasterPubKey);

    // p_recvdFrags[j][i] = p_secretFragments[i][j]
    vector<vector<bls::PrivateKey>> p_recvdFrags(N);
    for (int i = 0; i < N; ++i)
        for (int j = 0; j < N; ++j)
            p_recvdFrags[i].emplace_back(p_secretFragments[j][i]);

    vector<bls::PrivateKey> p_secretShares;
    for (int i = 0; i < N; ++i)
        p_secretShares.emplace_back(bls::PrivateKey::AggregateInsecure(p_recvdFrags[i]));



    uint8_t msg[] = {100, 2, 254, 88, 90, 45, 23};
    uint8_t hash[32];
    bls::Util::Hash256(hash, msg, sizeof(msg));

    size_t players[] = {1, 3};
    if (T == 2)
    {
        // For example, players 1 and 3 sign.
        // As we have verified the coefficients through the commitments given,
        // using InsecureSignature is okay.
        bls::InsecureSignature sigShareC1 = bls::Threshold::SignWithCoefficient(
            p_secretShares[0], msg, (size_t) sizeof(msg), (size_t) 1, players, T);
        bls::InsecureSignature sigShareC3 = bls::Threshold::SignWithCoefficient(
            p_secretShares[2], msg, (size_t) sizeof(msg), (size_t) 3, players, T);

        bls::InsecureSignature signature = bls::InsecureSignature::Aggregate({
            sigShareC1, sigShareC3});

        assert(signature.Verify({hash}, {masterPubkey}));

        std::cout << "OK!" << std::endl;
    }

    cout << "SIGNING ----------------" << endl;

    size_t total_signs = 0;
    auto start = std::chrono::steady_clock::now();
    vector<size_t> possible_signers(N);
    vector<bls::InsecureSignature> thrSignatures;
    for(size_t i = 0; i < N; ++i) possible_signers[i] = i + 1;
    do {
        vector<size_t> actual_signers(possible_signers.begin(), possible_signers.begin() + T);
        std::vector<bls::InsecureSignature> sigs;
        for(size_t i = 0; i < T; ++i)
        {
            bls::InsecureSignature sigShareCi = bls::Threshold::SignWithCoefficient(
                p_secretShares[actual_signers[i] - 1], msg, (size_t) sizeof(msg), actual_signers[i], actual_signers.data(), T);
            sigs.push_back(sigShareCi);
            ++total_signs;
        }
        bls::InsecureSignature signature = bls::InsecureSignature::Aggregate(sigs);
        thrSignatures.emplace_back(signature);
        //assert(signature.Verify({hash}, {masterPubkey}));
        if (total_signs > 1000)
            break;
    } while (std::next_permutation(possible_signers.begin(), possible_signers.end())); // Будут повторения, но в тестовых целях это не страшно
    auto finish = std::chrono::steady_clock::now();

    std::cout << "  OK!" << std::endl;
    {
        auto how_ms = std::chrono::duration_cast<std::chrono::milliseconds>(finish - start);
        auto how_us = std::chrono::duration_cast<std::chrono::microseconds>(finish - start);
        cout << "Total: " << total_signs << " runs in " << how_ms.count()
             << " ms" << endl;
        cout << "Avg: " << how_us.count() / total_signs
             << " us" << endl;
        cout << "Avg: " << 1000000000 / (how_us.count() / total_signs)
             << " signs per sec" << endl;
    }


    cout << "VERIFY SIGNS ----------------" << endl;
    start = std::chrono::steady_clock::now();
    for(auto const& thrSign: thrSignatures)
    {
        assert(thrSign.Verify({hash}, {masterPubkey}));
    }
    finish = std::chrono::steady_clock::now();
    {
        auto how_ms = std::chrono::duration_cast<std::chrono::milliseconds>(finish - start);
        auto how_us = std::chrono::duration_cast<std::chrono::microseconds>(finish - start);
        cout << "Total: " << total_signs << " runs in " << how_ms.count()
             << " ms" << endl;
        cout << "Avg: " << how_us.count() / total_signs
             << " us" << endl;
        cout << "Avg: " << 1000000000 / (how_us.count() / total_signs)
             << " signs per sec" << endl;
    }

    if (T == 2)
    {
        // For example, players 1 and 3 sign.
        bls::InsecureSignature sigShareU1 = p_secretShares[0].SignInsecure(
            msg, (size_t) sizeof(msg));
        bls::InsecureSignature sigShareU3 = p_secretShares[2].SignInsecure(
            msg, (size_t) sizeof(msg));

        bls::InsecureSignature sigShareU4 = p_secretShares[3].SignInsecure(
            msg, (size_t) sizeof(msg));

        bls::InsecureSignature signature2 = bls::Threshold::AggregateUnitSigs(
            {sigShareU1, sigShareU3, sigShareU4}, msg, (size_t) sizeof(msg), players, T);

        assert(signature2.Verify({hash}, {masterPubkey}));

        std::cout << "OK 3 !" << std::endl;
    }

}


int
main()
{
    cout << "Starting!" << endl;

    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    // Example seed, used to generate private key. Always use
    // a secure RNG with sufficient entropy to generate a seed.
    uint8_t seed[] = {0, 50, 6, 244, 24, 199, 1, 25, 52, 88, 192,
                      19, 18, 12, 89, 6, 220, 18, 102, 58, 209,
                      82, 12, 62, 89, 110, 182, 9, 44, 20, 254, 22};

    /*

    bls::PrivateKey sk = bls::PrivateKey::FromSeed(seed, sizeof(seed));
    bls::PublicKey pk = sk.GetPublicKey();

    uint8_t msg[] = {100, 2, 254, 88, 90, 45, 23};

    bls::Signature sig = sk.Sign(msg, sizeof(msg));


    // Serializing

    uint8_t skBytes[bls::PrivateKey::PRIVATE_KEY_SIZE];  // 32 byte array
    uint8_t pkBytes[bls::PublicKey::PUBLIC_KEY_SIZE];    // 48 byte array
    uint8_t sigBytes[bls::Signature::SIGNATURE_SIZE];    // 96 byte array

    sk.Serialize(skBytes);   // 32 bytes
    pk.Serialize(pkBytes);   // 48 bytes
    sig.Serialize(sigBytes); // 96 bytes

    cout << "Secret Key:" << endl;
    for (auto b: skBytes) cout << "0x" << hex << (int)b << " ";
    cout << "\n";
    cout << "Public Key:" << endl;
    for (auto b: pkBytes) cout << "0x" << hex << (int)b << " ";
    cout << "\n";
    cout << "Signature:" << endl;
    for (auto b: sigBytes) cout << "0x" << hex << (int)b << " ";
    cout << "\n";

    // Verifying:
    sig.SetAggregationInfo(bls::AggregationInfo::FromMsg(pk, msg, sizeof(msg)));
    bool ok = sig.Verify();
    if (ok)
        cout << "Sign ok!" << endl;
    else
        cout << "Sign NOT ok!" << endl;
         */

    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    // Benchmarking:

    size_t const N = 10000;
    size_t const MESSAGE_SIZE = 1024;
    size_t const AGG_N = 313;
    size_t const NA = N / AGG_N;

    cout << "Generating keys..." << endl;

    bls::PrivateKey sk = bls::PrivateKey::FromSeed(seed, sizeof(seed));
    bls::PublicKey pk = sk.GetPublicKey();

    // bls::Signature sig = sk.Sign(msg, sizeof(msg));

    cout << "Generating test messages..." << endl;
    srand(1);
    vector<vector<uint8_t>> messages(N);
    vector<vector<uint8_t>> hashes(N);
    for(size_t i = 0; i < N; ++i)
    {
        vector<uint8_t> msg(MESSAGE_SIZE);
        for(auto& b: msg)
            b = rand();
        messages[i] = msg;
        hashes[i].resize(32);
        bls::Util::Hash256(hashes[i].data(), msg.data(), msg.size());
    }
    cout << "    ok" << endl;

    vector<bls::InsecureSignature> sigs;
    {
        cout << "Signing test messages..." << endl;
        auto start = std::chrono::steady_clock::now();
        for(size_t i = 0; i < N; ++i)
        {
            //sigs[i].reset(new bls::Signature(sk.Sign(messages[i].data(), MESSAGE_SIZE)));
            sigs.emplace_back(sk.SignInsecurePrehashed(hashes[i].data()));
        }
        //...
        auto finish = std::chrono::steady_clock::now();
        auto how_ms = std::chrono::duration_cast<std::chrono::milliseconds>(finish - start);
        auto how_us = std::chrono::duration_cast<std::chrono::microseconds>(finish - start);
        cout << "Total: " << N << " runs in " << how_ms.count()
             << " ms" << endl;
        cout << "Avg: " << how_us.count() / N
             << " us" << endl;
        cout << "Avg: " << 1000000 / (how_us.count() / N)
             << " signs per sec" << endl;
    }

    vector<bls::InsecureSignature> aggsigs;
    {
        cout << "Aggregate signatures..." << endl;
        auto start = std::chrono::steady_clock::now();
        for(size_t i = 0; i < NA; ++i)
        {
            //sigs[i].reset(new bls::Signature(sk.Sign(messages[i].data(), MESSAGE_SIZE)));
            auto somesigs = vector<bls::InsecureSignature>(sigs.begin() + i * AGG_N, sigs.begin() + (i + 1) * AGG_N);
            aggsigs.emplace_back(bls::InsecureSignature(bls::InsecureSignature::Aggregate(somesigs)));
        }
        //...
        auto finish = std::chrono::steady_clock::now();
        auto how_ms = std::chrono::duration_cast<std::chrono::milliseconds>(finish - start);
        auto how_us = std::chrono::duration_cast<std::chrono::microseconds>(finish - start);
        cout << "Total: " << NA << " runs in " << how_ms.count()
             << " ms" << endl;
        cout << "Avg: " << how_us.count() / NA
             << " us" << endl;
        cout << "Avg: " << 1000000 / (how_us.count() / NA)
             << " aggregations per sec" << endl;
    }



    // ~~~~~~~~~~~~
    




    // THRESHOLD

//    test_thresh(20, 30);

    /*
    cout << "~~~~~~~~~~~~~~~\n";

    vector<bls::PublicKey>  commitment;
    vector<bls::PrivateKey> secretFragment;
    size_t const T = 7;
    size_t const N = 9;
    for(size_t i = 0; i < N; ++i)
    {
        g1_t g;
        commitment.emplace_back(bls::PublicKey::FromG1(&g));
        bn_t b;
        bn_new(b);
        secretFragment.emplace_back(bls::PrivateKey::FromBN(b));
    }
    auto const player_secret_key = bls::Threshold::Create(commitment, secretFragment, T, N);
    // --
    bool r = bls::Threshold::VerifySecretFragment(1, secretFragment[0], commitment, T);
    if (r)
        cout << "OK\n";
    else
        cout << "wrong\n";

    return 0;
    */
}
