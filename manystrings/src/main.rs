struct ManyStrings {
    field1: String,
    field2: String,
    field3: String,
}
struct CompactManyStrings {
    string_contents: Box<str>,
    field1_end: u32,
    field2_end: u32,
}
impl CompactManyStrings {
    fn get_field1(&self) -> &str {
        &self.string_contents[0..(self.field1_end as usize)]
    }
    fn get_field2(&self) -> &str {
        &self.string_contents[(self.field1_end as usize)..(self.field2_end as usize)]
    }
    fn get_field3(&self) -> &str {
        &self.string_contents[(self.field2_end as usize)..self.string_contents.len()]
    }
    fn new(field1: &str, field2: &str, field3: &str) -> Self {
        let mut string_contents = String::with_capacity(field1.len() + field2.len() + field3.len());
        string_contents.push_str(field1);
        string_contents.push_str(field2);
        string_contents.push_str(field3);
        Self {
            string_contents: string_contents.into_boxed_str(),
            field1_end: field1.len() as u32,
            field2_end: (field1.len() + field2.len()) as u32,
        }
    }
}
fn main() {
    println!("Hello, world!");
    let mstrings = ManyStrings {
        field1: "burp".to_string(),
        field2: "simon".to_string(),
        field3: "fleet".to_string(),
    };
    println!(
        "wide, field1: {}, field2: {}, field3: {}",
        mstrings.field1, mstrings.field2, mstrings.field3,
    );
    let cmstrings = CompactManyStrings::new(
        "burp",  //
        "simon", //
        "fleet", //
    );
    println!(
        "compact, field1: {}, field2: {}, field3: {}",
        cmstrings.get_field1(),
        cmstrings.get_field2(),
        cmstrings.get_field3()
    );
}
