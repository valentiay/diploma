db.createUser(
        {
            user: "thesis",
            pwd: "password",
            roles: [
                {
                    role: "readWrite",
                    db: "thesis"
                }
            ]
        }
);