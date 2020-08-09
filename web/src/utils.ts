export const unwrap = <T>(nullable: T | null): T => {
    if (nullable == undefined) throw new Error('unable to unwrap undefined')
    return nullable
}
